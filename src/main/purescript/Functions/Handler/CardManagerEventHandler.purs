module Functions.Handler.CardManagerEventHandler
  ( getCardSteps
  , handleCardManagerEvent
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Category ((<<<))
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Data.Function ((#), ($))
import Data.HeytingAlgebra (not)
import Data.Lens (view)
import Data.List (List(..), (:))
import Data.Map (insert, lookup)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), uncurry)
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.AppState (AppState, CardsCache)
import DataModel.CardVersions.Card as DataModel.CardVersions.Card
import DataModel.Communication.ConnectionState (ConnectionState)
import DataModel.FragmentState as Fragment
import DataModel.IndexVersions.Index (CardEntry(..), _cardReference_reference, addToIndex, reference, removeFromIndex)
import DataModel.Proxy (ProxyInfo, ProxyResponse(..))
import DataModel.UserVersions.User (UserInfo(..), _indexReference_refence, _userInfoReference_reference)
import DataModel.WidgetState (CardFormInput(..), CardManagerState, CardViewState(..), Page(..), WidgetState(..), MainPageWidgetState)
import Effect.Aff.Class (liftAff)
import Functions.Card (appendToTitle, archiveCard, decryptCard, restoreCard)
import Functions.Communication.Blobs (getBlob)
import Functions.Communication.Cards (deleteCard, postCard)
import Functions.Communication.Users (asMaybe, computeRemoteUserCard)
import Functions.Handler.DonationEventHandler (handleDonationPageEvent)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, handleOperationResult, noOperation, runStep, runWidgetStep)
import Functions.Index (updateIndex)
import OperationalWidgets.Sync (SyncOperation(..), addPendingOperations)
import Record (merge)
import Views.CardsManagerView (CardManagerEvent(..), NavigateCardsEvent(..))
import Views.CreateCardView (CardFormData, emptyCardFormData)
import Views.DonationViews as DonationEvent
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.UserAreaView (userAreaInitialState)

handleCardManagerEvent :: CardManagerEvent -> CardManagerState -> AppState -> ProxyInfo -> Fragment.FragmentState -> Widget HTML OperationState
handleCardManagerEvent cardManagerEvent cardManagerState state@{index: Just index, userInfo: Just userInfo@(UserInfo {userPreferences, donationInfo}), userInfoReferences: Just userInfoReferences, proxy, srpConf, hash: hashFunc, c: Just c, s: Just s, p: Just p, username: Just username, password: Just password, pinEncryptedPassword, enableSync, cardsCache, syncDataWire, donationLevel: Just donationLevel} proxyInfo f = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}
  
  let defaultPage = { index
                    , credentials:      {username, password}
                    , donationInfo
                    , pinExists:        isJust pinEncryptedPassword
                    , enableSync
                    , userPreferences
                    , userAreaState: userAreaInitialState
                    , cardManagerState
                    , donationLevel
                    , syncDataWire: Just syncDataWire
                    }

  case cardManagerEvent of

    (OpenUserAreaEvent) -> 
      noOperation (Tuple 
                  state
                  (WidgetState
                    hiddenOverlayInfo
                    (Main defaultPage { userAreaState = userAreaInitialState {showUserArea = true} }
                    )
                    proxyInfo
                  )
                )

    (ShowShortcutsEvent show) ->
      updateCardManagerState defaultPage cardManagerState {showShortcutsHelp = show}
    
    (ShowDonationEvent show) ->
      updateCardManagerState defaultPage cardManagerState {showDonationOverlay = show}

    (UpdateDonationLevel days) -> handleDonationPageEvent (DonationEvent.UpdateDonationLevel days) state proxyInfo f

    (ChangeFilterEvent filterData) ->
      updateCardManagerState defaultPage cardManagerState { filterData = filterData
                                                          , highlightedEntry = Nothing 
                                                          }

    (UpdateCardForm cardFormData) ->
      updateCardManagerState defaultPage cardManagerState { cardViewState = updateCardViewState cardManagerState.cardViewState cardFormData }

    (NavigateCardsEvent navigationEvent) ->
      case navigationEvent of
        Move             i -> updateCardManagerState defaultPage (cardManagerState {                                 highlightedEntry = Just i })
        Close       maybei -> updateCardManagerState defaultPage (cardManagerState {cardViewState = NoCard,          highlightedEntry = maybei })
        Open   Nothing     -> doNothing defaultPage
        Open  (Just entry) -> do
          ProxyResponse proxy' (Tuple cardsCache' card) <- getCardSteps connectionState cardsCache entry (Main defaultPage) proxyInfo
          pure (Tuple
                  state {proxy = proxy', cardsCache = cardsCache'}
                  (WidgetState
                    hiddenOverlayInfo
                    (Main defaultPage { cardManagerState =  cardManagerState {cardViewState = Card card entry, highlightedEntry = Nothing} }
                    )
                    proxyInfo
                  )
                )
          # runExceptT
          >>= handleOperationResult state defaultErrorPage (isNothing $ lookup (reference entry) cardsCache) Black
    
    (OpenCardFormEvent maybeCard) ->
      updateCardManagerState defaultPage cardManagerState { cardViewState = uncurry CardForm $ case maybeCard of
                                                              Nothing                     -> Tuple  emptyCardFormData                 NewCard
                                                              Just (Tuple cardEntry card) -> Tuple (emptyCardFormData {card = card}) (ModifyCard card cardEntry)
                                                          }

    (AddCardEvent card) ->
      do
        res <- addCardSteps cardManagerState state card (Main $ defaultPage {cardManagerState = cardManagerState {cardViewState = CardForm (emptyCardFormData {card = card}) (NewCardFromFragment card)}}) proxyInfo "Add card" 
        pure res
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

    (CloneCardEvent cardEntry) ->
      do
        ProxyResponse proxy' (Tuple cardsCache' card) <- getCardSteps connectionState cardsCache cardEntry (Main defaultPage) proxyInfo
        let cloneCard                                  = appendToTitle " - copy" <<< (\(DataModel.CardVersions.Card.Card card') -> DataModel.CardVersions.Card.Card card' {archived = false}) $ card
        res                                           <- addCardSteps cardManagerState state{proxy = proxy', cardsCache = cardsCache'} cloneCard (Main defaultPage) proxyInfo "Clone card"
        pure res

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black
  
    (DeleteCardEvent cardEntry) ->
      do
        updatedIndex                          <- runStep ((removeFromIndex cardEntry index) # liftAff)                                               (WidgetState (spinnerOverlay "Update index" Black) (Main defaultPage) proxyInfo)
        ProxyResponse proxy'  stateUpdateInfo <- runStep (updateIndex state updatedIndex)                                                            (WidgetState (spinnerOverlay "Update index" Black) (Main defaultPage) proxyInfo)
        ProxyResponse proxy'' cardsCache'     <- runStep (deleteCard (connectionState {proxy = proxy'}) cardsCache (unwrap cardEntry).cardReference) (WidgetState (spinnerOverlay "Delete card"  Black) (Main defaultPage) proxyInfo)


        syncOperations <- runStep (if (not enableSync) then (pure Nil) else do
                            user <- computeRemoteUserCard c p s stateUpdateInfo.masterKey srpConf
                            pure  ( (SaveBlob   $ view _indexReference_refence      stateUpdateInfo.userInfo          )
                                  : (SaveBlob   $ view _userInfoReference_reference stateUpdateInfo.userInfoReferences)
                                  : (SaveUser     user                                                )
                                  : (DeleteBlob $ view _userInfoReference_reference userInfoReferences)
                                  : (DeleteBlob $ view _indexReference_refence      userInfo          )
                                  : (DeleteBlob $ view _cardReference_reference     cardEntry         )
                                  :  Nil
                                  )
                          ) (WidgetState (spinnerOverlay "Compute data to sync" Black) (Main defaultPage) proxyInfo)
  
        _              <- runWidgetStep (addPendingOperations syncDataWire syncOperations) (WidgetState (spinnerOverlay "Compute data to sync" Black) (Main defaultPage) proxyInfo)

        pure (Tuple 
                (merge (asMaybe stateUpdateInfo) state {proxy = proxy'', index = Just updatedIndex, cardsCache = cardsCache'})
                (WidgetState
                  hiddenOverlayInfo
                  (Main defaultPage { index            = updatedIndex
                                    , cardManagerState = cardManagerState {cardViewState = NoCard, highlightedEntry = Nothing}
                                    }
                  )
                  proxyInfo
                )
            )

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

    (EditCardEvent (Tuple oldCardEntry updatedCard)) ->
      do
        editCardSteps cardManagerState state oldCardEntry updatedCard (Main defaultPage {cardManagerState = cardManagerState {cardViewState = CardForm (emptyCardFormData {card = updatedCard}) (ModifyCard updatedCard oldCardEntry) }}) proxyInfo
      
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

    (ArchiveCardEvent cardEntry) ->
      do
        ProxyResponse proxy' (Tuple _ card) <- getCardSteps  connectionState  cardsCache            cardEntry             (Main defaultPage) proxyInfo
        let updatedCard                      = archiveCard card
        res                                 <- editCardSteps cardManagerState state{proxy = proxy'} cardEntry updatedCard (Main defaultPage) proxyInfo
        pure res

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black
    
    (RestoreCardEvent cardEntry) ->
      do
        ProxyResponse proxy' (Tuple _ card) <- getCardSteps  connectionState  cardsCache            cardEntry             (Main defaultPage) proxyInfo
        let updatedCard                      = restoreCard card
        res                                 <- editCardSteps cardManagerState state{proxy = proxy'} cardEntry updatedCard (Main defaultPage) proxyInfo
        pure res

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

  where
    updateCardManagerState :: MainPageWidgetState -> CardManagerState -> Widget HTML OperationState
    updateCardManagerState mainPageState cardManagerState' = 
      noOperation (Tuple 
                    state
                    (WidgetState
                      hiddenOverlayInfo
                      (Main mainPageState { cardManagerState = cardManagerState' }
                      )
                      proxyInfo
                    )
                  )
    
    doNothing :: MainPageWidgetState -> Widget HTML OperationState
    doNothing mainPageState =  noOperation (Tuple state (WidgetState hiddenOverlayInfo (Main mainPageState) proxyInfo))

    updateCardViewState :: CardViewState -> CardFormData -> CardViewState
    updateCardViewState (CardForm _ cardFormInput) cardFormData = CardForm cardFormData cardFormInput
    updateCardViewState  cardViewState             _            = cardViewState

handleCardManagerEvent _ _ state _ _ = do
  throwError $ InvalidStateError (CorruptedState "cardManagerEvent")
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true Black

-- ===================================================================================================

getCardSteps :: ConnectionState -> CardsCache -> CardEntry -> Page -> ProxyInfo -> ExceptT AppError (Widget HTML) (ProxyResponse (Tuple CardsCache DataModel.CardVersions.Card.Card))
getCardSteps connectionState cardsCache cardEntry@(CardEntry entry) page proxyInfo = do
  let cardFromCache = lookup (reference cardEntry) cardsCache
  case cardFromCache of
    Just card -> pure $ ProxyResponse connectionState.proxy (Tuple cardsCache card)
    Nothing   -> do
      ProxyResponse proxy' blob <- runStep (getBlob connectionState (reference cardEntry)) (WidgetState (spinnerOverlay "Get card"     Black) page proxyInfo)  
      card                      <- runStep (decryptCard blob (entry.cardReference))        (WidgetState (spinnerOverlay "Decrypt card" Black) page proxyInfo)
      let updatedCardsCache      =          insert (reference cardEntry) card cardsCache
      pure $ ProxyResponse proxy' (Tuple updatedCardsCache card)

addCardSteps :: CardManagerState -> AppState -> DataModel.CardVersions.Card.Card -> Page -> ProxyInfo -> String -> ExceptT AppError (Widget HTML) OperationState
addCardSteps cardManagerState state@{index: Just index, userInfo: Just userInfo@(UserInfo {userPreferences, donationInfo}), userInfoReferences: Just userInfoReferences, proxy, hash: hashFunc, srpConf, c: Just c, s: Just s, p: Just p, cardsCache, username: Just username, password: Just password, pinEncryptedPassword, enableSync, syncDataWire, donationLevel: Just donationLevel} newCard page proxyInfo message = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}
  ProxyResponse proxy'  (Tuple cardsCache' newCardEntry) <- runStep (postCard connectionState cardsCache newCard)       (WidgetState (spinnerOverlay message        Black) page proxyInfo)
  updatedIndex                                           <- runStep (addToIndex newCardEntry index # liftAff)           (WidgetState (spinnerOverlay "Update index" Black) page proxyInfo)
  ProxyResponse proxy''  stateUpdateInfo                 <- runStep (updateIndex (state {proxy = proxy'}) updatedIndex) (WidgetState (spinnerOverlay "Update index" Black) page proxyInfo)

  syncOperations <- runStep (if (not enableSync) then (pure Nil) else do
                      user <- computeRemoteUserCard c p s stateUpdateInfo.masterKey srpConf
                      pure  ( (SaveBlob   $ view _cardReference_reference     newCardEntry                      )
                            : (SaveBlob   $ view _indexReference_refence      stateUpdateInfo.userInfo          )
                            : (SaveBlob   $ view _userInfoReference_reference stateUpdateInfo.userInfoReferences)
                            : (SaveUser     user                                            )
                            : (DeleteBlob $ view _userInfoReference_reference userInfoReferences)
                            : (DeleteBlob $ view _indexReference_refence      userInfo          )
                            :  Nil
                            )
                    ) (WidgetState (spinnerOverlay "Compute data to sync" Black) page proxyInfo)
  
  _               <- runWidgetStep (addPendingOperations syncDataWire syncOperations) (WidgetState (spinnerOverlay "Compute data to sync" Black) page proxyInfo)

  pure (Tuple 
          (merge (asMaybe stateUpdateInfo) state  {proxy = proxy'', index = Just updatedIndex, cardsCache = cardsCache'})
          (WidgetState
            hiddenOverlayInfo
            (Main { index:            updatedIndex
                  , credentials:     {username, password}
                  , donationInfo
                  , pinExists: isJust pinEncryptedPassword
                  , enableSync
                  , userPreferences
                  , userAreaState:    userAreaInitialState
                  , cardManagerState: cardManagerState {cardViewState = Card newCard newCardEntry, highlightedEntry = Nothing}
                  , donationLevel
                  , syncDataWire: Just syncDataWire
                  }
            )
            proxyInfo
          )
        )
addCardSteps _ _ _ _ _ _ = throwError $ InvalidStateError (CorruptedState "addCardStep")

editCardSteps :: CardManagerState -> AppState -> CardEntry -> DataModel.CardVersions.Card.Card -> Page -> ProxyInfo -> ExceptT AppError (Widget HTML) OperationState
editCardSteps cardManagerState state@{index: Just index, proxy, srpConf, hash: hashFunc, c: Just c, s: Just s, p: Just p, userInfo: Just userInfo@(UserInfo {userPreferences, donationInfo}), userInfoReferences: Just userInfoReferences, cardsCache, username: Just username, password: Just password, pinEncryptedPassword, donationLevel: Just donationLevel, enableSync, syncDataWire} oldCardEntry updatedCard page proxyInfo = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}
  ProxyResponse proxy'   (Tuple cardsCache' cardEntry) <- runStep       (postCard connectionState cardsCache updatedCard)                                                 (WidgetState (spinnerOverlay "Post updated card" Black) page proxyInfo)
  updatedIndex                                         <- runStep       ((addToIndex cardEntry index >>= removeFromIndex oldCardEntry) # liftAff)                         (WidgetState (spinnerOverlay "Update index"      Black) page proxyInfo)
  ProxyResponse proxy''   stateUpdateInfo              <- runStep       (updateIndex (state {proxy = proxy'}) updatedIndex)                                               (WidgetState (spinnerOverlay "Update index"      Black) page proxyInfo)
  ProxyResponse proxy'''  cardsCache''                 <- runStep       (deleteCard  (connectionState {proxy = proxy''}) cardsCache' (unwrap oldCardEntry).cardReference) (WidgetState (spinnerOverlay "Delete old card"   Black) page proxyInfo)

  syncOperations <- runStep (if (not enableSync) then (pure Nil) else do
                      user <- computeRemoteUserCard c p s stateUpdateInfo.masterKey srpConf
                      pure  ( (SaveBlob   $ view _cardReference_reference     cardEntry                         )
                            : (SaveBlob   $ view _indexReference_refence      stateUpdateInfo.userInfo          )
                            : (SaveBlob   $ view _userInfoReference_reference stateUpdateInfo.userInfoReferences)
                            : (SaveUser     user                                                )
                            : (DeleteBlob $ view _userInfoReference_reference userInfoReferences)
                            : (DeleteBlob $ view _indexReference_refence      userInfo          )
                            : (DeleteBlob $ view _cardReference_reference     oldCardEntry      )
                            :  Nil
                            )
                    ) (WidgetState (spinnerOverlay "Compute data to sync" Black) page proxyInfo)

  _              <- runWidgetStep (addPendingOperations syncDataWire syncOperations)                                                (WidgetState (spinnerOverlay "Compute data to sync" Black) page proxyInfo)

  pure  (Tuple 
          (merge (asMaybe stateUpdateInfo) state {proxy = proxy''', index = Just updatedIndex, cardsCache = cardsCache''})
          (WidgetState
            hiddenOverlayInfo
            (Main { index:            updatedIndex
                  , credentials:     {username, password}
                  , donationInfo
                  , pinExists: isJust pinEncryptedPassword
                  , enableSync
                  , userPreferences
                  , userAreaState:    userAreaInitialState
                  , cardManagerState: cardManagerState {cardViewState = (Card updatedCard cardEntry), highlightedEntry = Nothing}
                  , donationLevel
                  , syncDataWire: Just syncDataWire
                  }
            )
            proxyInfo
          )
        )
editCardSteps _ _ _ _ _ _ = throwError $ InvalidStateError (CorruptedState "editCardStep")


-- ========================================================================================================================

-- runStepWithStateT :: forall a. ExceptT AppError (StateT AppState Aff) a -> WidgetState -> ExceptT AppError (StateT AppState (Widget HTML)) a
-- runStepWithStateT step widgetState = step # mapExceptT (mapStateT (\res -> liftAff res <|> (defaultView widgetState)))

-- handleOperationResultWithStateT :: Page -> Tuple (Either AppError WidgetState) AppState -> Widget HTML OperationState
-- handleOperationResultWithStateT page res = swap res <#> either
--                                             manageError
--                                             (\widgetState@(WidgetState _ page') -> delayOperation 500 (WidgetState { status: Done, message: "" } page') *> pure widgetState)
--                                          # sequence
                                                
--   where
--     manageError :: AppError -> Widget HTML WidgetState
--     manageError error = 
--       case error of
--         -- _ -> ErrorPage --TODO
--         err -> do
--           liftEffect $ log $ show err
--           delayOperation 500 (WidgetState { status: Failed,  message: "error" } page)
--           pure               (WidgetState { status: Hidden,  message: ""      } page)

-- addCardStepsWithStateT :: DataModel.CardVersions.Card.Card -> Page -> String -> ExceptT AppError (StateT AppState (Widget HTML)) WidgetState
-- addCardStepsWithStateT newCard page message = do
--   {index, userPreferences, cardsCache} <- get
--   index'                               <-  except $ note (InvalidStateError $ CorruptedState "index not found")             index
--   userPasswordGeneratorSettings        <-  except $ note (InvalidStateError $ CorruptedState "user preferences not found") (userPreferences <#> (\up -> (unwrap up).passwordGeneratorSettings))

--   newCardEntry                          <- runStepWithStateT (postCardWithStateT newCard)         (WidgetState (spinnerOverlay message)        page)
--   let updatedIndex                       = addToIndex newCardEntry index'
--   _                                     <- runStepWithStateT (updateIndexWithStateT updatedIndex) (WidgetState (spinnerOverlay "Update index") page)
--   let updatedCardCache                   = insert (reference newCardEntry) newCard cardsCache
  
--   modify_ (\state -> state {cardsCache = updatedCardCache})

--   pure  (WidgetState
--           hiddenOverlayInfo
--           (Main { index: updatedIndex
--                 , showUserArea: false
--                 , cardsManagerState: cardManagerInitialState {cardViewState = Card newCard newCardEntry, selectedEntry = Just newCardEntry}
--                 , userPasswordGeneratorSettings 
--                 }
--           )
--         )

-- postCardWithStateT :: DataModel.CardVersions.Card.Card -> ExceptT AppError (StateT AppState Aff) CardEntry
-- postCardWithStateT card = do
--   {proxy, hash} <- get
--   key <- liftAff $ KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
--   Tuple encryptedCard cardEntry@(CardEntry {cardReference: CardReference {reference}}) <- liftAff $ createCardEntry card key hashFuncSHA256
--   ProxyResponse proxy' _ <- mapExceptT lift $ postBlob {proxy, hashFunc: hash} encryptedCard (toArrayBuffer reference)
--   modify_ (\state -> state {proxy = proxy'})
--   pure cardEntry
 
-- updateIndexWithStateT :: Index -> ExceptT AppError (StateT AppState Aff) Unit
-- updateIndexWithStateT newIndex = do
--   state <- get
--   case state of
--     { c: Just c, p: Just p, userInfoReferences: Just (UserInfoReferences r@{ indexReference: (IndexReference oldReference) }), masterKey: Just originMasterKey, proxy, hash: hashFunc, index: Just index } -> do
--       cryptoKey            :: CryptoKey   <- liftAff $ importCryptoKeyAesGCM (toArrayBuffer oldReference.masterKey)
--       indexCardContent     :: ArrayBuffer <- liftAff $ encryptJson cryptoKey newIndex
--       indexCardContentHash :: ArrayBuffer <- liftAff $ hashFunc (indexCardContent : Nil)
--       ProxyResponse proxy'   _            <- mapExceptT lift $ postBlob {proxy, hashFunc} indexCardContent indexCardContentHash
--       modify_ (\state_ -> state_ {proxy = proxy'})
--       -- -------------------
--       let newIndexReference                = IndexReference $ oldReference { reference = fromArrayBuffer indexCardContentHash }
--       let newUserInfoReferences            = UserInfoReferences r { indexReference = newIndexReference }
--       masterPassword       :: CryptoKey   <- liftAff $ importCryptoKeyAesGCM (toArrayBuffer p)
--       masterKeyContent     :: HexString   <- liftAff $ fromArrayBuffer <$> encryptJson masterPassword newUserInfoReferences
--       let newUserCard                      = UserCard { masterKey: Tuple masterKeyContentMasterKeyEncodingVersion_1, originMasterKey: fst originMasterKey }
--       ProxyResponse proxy'' newMasterKey  <- mapExceptT lift $ updateUserCard {proxy: proxy', hashFunc} c newUserCard
--       modify_ (\state_ -> state_ {proxy = proxy''})
--       -- -------------------
--       oldIndexCartContent  :: ArrayBuffer <- liftAff $ encryptJson cryptoKey index

--       ProxyResponse proxy''' _            <- mapExceptT lift $ deleteBlob {proxy: proxy'', hashFunc} oldIndexCartContent oldReference.reference
--       modify_ (\state_ -> state_ { proxy = proxy'''
--                                , index = Just newIndex
--                                , userInfoReferences = Just newUserInfoReferences
--                                , masterKey = Just newMasterKey
--                                })

--     _ -> throwError $ InvalidStateError (MissingValue "Missing p, c or indexReference")
