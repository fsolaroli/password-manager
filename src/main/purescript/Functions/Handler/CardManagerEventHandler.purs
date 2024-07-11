module Functions.Handler.CardManagerEventHandler
  ( getCardSteps
  , handleCardManagerEvent
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML, affAction)
import Control.Alt (void, (<#>), (<$>))
import Control.Alternative ((*>), (<*))
import Control.Applicative (pure)
import Control.Bind (bind, (=<<), (>>=))
import Control.Category ((<<<))
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Data.Function (flip, (#), ($), (>>>))
import Data.HexString (fromArrayBuffer)
import Data.Identifier (computeIdentifier)
import Data.Lens (set, view)
import Data.List (List(..), singleton, (:))
import Data.Map (delete, insert, lookup)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, uncurry)
import Data.Unit (unit)
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.AppState (AppState, CardsCache)
import DataModel.CardVersions.Card as DataModel.CardVersions.Card
import DataModel.Communication.ConnectionState (ConnectionState)
import DataModel.FragmentState as Fragment
import DataModel.IndexVersions.Index (CardEntry(..), Index, _card_identifier, _card_reference, _index_identifier, addToIndex, reference, removeFromIndex)
import DataModel.Proxy (ProxyInfo, ProxyResponse(..))
import DataModel.SRPVersions.SRP (hashFuncSHA256)
import DataModel.UserVersions.User (UserInfo(..), _indexReference, _index_reference, _userInfo_identifier, _userInfo_reference)
import DataModel.WidgetState (CardFormInput(..), CardViewState(..), MainPageWidgetState, Page(..), WidgetState(..), CardManagerState)
import Effect.Aff (Milliseconds(..), delay, forkAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Card (appendToTitle, archiveCard, createCardEntry, decryptCard, restoreCard)
import Functions.Communication.Blobs (getBlob)
import Functions.Communication.SyncBackend (syncBackend)
import Functions.Communication.Users (computeMasterKey, computeRemoteUserCard, encryptUserInfo)
import Functions.Events (blur, focus, scrollElementIntoView, select)
import Functions.Handler.DonationEventHandler (handleDonationPageEvent)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, handleOperationResult, noOperation, runStep, syncLocalStorage)
import Functions.Index (encryptIndex)
import IndexFilterView (Filter(..), FilterViewStatus(..))
import OperationalWidgets.Sync (SyncOperation(..))
import Views.CardsManagerView (CardManagerEvent(..), NavigateCardsEvent(..))
import Views.CreateCardView (CardFormData, emptyCardFormData)
import Views.DonationViews as DonationEvent
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.UserAreaView (userAreaInitialState)

handleCardManagerEvent :: CardManagerEvent -> CardManagerState -> AppState -> ProxyInfo -> Fragment.FragmentState -> Widget HTML OperationState
handleCardManagerEvent cardManagerEvent cardManagerState state@{index: Just index, userInfo: Just (UserInfo {userPreferences, donationInfo}), proxy, srpConf, hash: hashFunc, c: Just c, p: Just p, username: Just username, password: Just password, pinEncryptedPassword, enableSync, cardsCache, syncDataWire, donationLevel: Just donationLevel} proxyInfo f = do
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
    
    (NoEvent) -> noOperation (Tuple state (WidgetState hiddenOverlayInfo (Main defaultPage) proxyInfo))

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
      <* (forkAff ((delay (Milliseconds 10.0)) *> (focus "userPage" # liftEffect)) # affAction)

    (ShowShortcutsEvent show) ->
      updateCardManagerState defaultPage cardManagerState {showShortcutsHelp = show}
    
    (ShowDonationEvent show) ->
      (if show then (pure unit) else focus "mainView" # liftEffect)
      *>
      updateCardManagerState defaultPage cardManagerState {showDonationOverlay = show}

    (UpdateDonationLevel days) -> handleDonationPageEvent (DonationEvent.UpdateDonationLevel days) state proxyInfo f

    (ChangeFilterEvent filterData) ->
      updateCardManagerState defaultPage cardManagerState { filterData = filterData {selected = false}
                                                          , highlightedEntry = Nothing 
                                                          }
      <* case filterData.filterViewStatus of
          FilterViewOpen   -> 
            case filterData.filter of
              Search _     -> forkAff ((delay (Milliseconds 10.0)) *> ((if filterData.selected then select "searchInputField" else focus "searchInputField") # liftEffect)) # void # affAction
              _            -> focus "filterView" # liftEffect
          FilterViewClosed -> focus "mainView"   # liftEffect

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
        <* ((forkAff ((delay (Milliseconds 10.0)) *> (scrollElementIntoView "selectedCard" # liftEffect))) # affAction)
        <* (focus "mainView" # liftEffect)
    
    (OpenCardFormEvent maybeCard) ->
      updateCardManagerState defaultPage cardManagerState { cardViewState = uncurry CardForm $ case maybeCard of
                                                              Nothing                     -> Tuple  emptyCardFormData                 NewCard
                                                              Just (Tuple cardEntry card) -> Tuple (emptyCardFormData {card = card}) (ModifyCard card cardEntry)
                                                          }
      <* (blur "mainView" # liftEffect)

    (AddCardEvent card) ->
      (cardOperationSteps (Add card) (spinnerWidgetState (Main $ defaultPage {cardManagerState = cardManagerState {cardViewState = CardForm (emptyCardFormData {card = card}) (NewCardFromFragment card)}})) cardManagerState state <#> wrapResponse
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black
      )
      <* (focus "mainView" # liftEffect)

    (CloneCardEvent cardEntry) ->
      (focus "mainView" # liftEffect)
      *>
      do
        ProxyResponse proxy' (Tuple cardsCache' card) <- getCardSteps connectionState cardsCache cardEntry (Main defaultPage) proxyInfo
        let cloneCard                                  = appendToTitle " - copy" <<< (\(DataModel.CardVersions.Card.Card card') -> DataModel.CardVersions.Card.Card card' {archived = false}) $ card
        res                                           <- cardOperationSteps (Add cloneCard) (spinnerWidgetState (Main defaultPage)) cardManagerState state{proxy = proxy', cardsCache = cardsCache'}
        pure $ wrapResponse res

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black
  
    (DeleteCardEvent cardEntry) ->
      (focus "mainView" # liftEffect)
      *>
      do
        cardOperationSteps (Delete cardEntry) (spinnerWidgetState (Main defaultPage)) cardManagerState state <#> wrapResponse

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

    (EditCardEvent (Tuple oldCardEntry updatedCard)) ->
      (focus "mainView" # liftEffect)
      *>
      do
        cardOperationSteps (Edit updatedCard oldCardEntry) (spinnerWidgetState (Main defaultPage {cardManagerState = cardManagerState {cardViewState = CardForm (emptyCardFormData {card = updatedCard}) (ModifyCard updatedCard oldCardEntry) }})) cardManagerState state <#> wrapResponse
      
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

    (ArchiveCardEvent cardEntry) ->
      (focus "mainView" # liftEffect)
      *>
      do
        ProxyResponse proxy' (Tuple _ card) <- getCardSteps  connectionState  cardsCache            cardEntry             (Main defaultPage) proxyInfo
        let updatedCard                      = archiveCard card
        res                                 <- cardOperationSteps (Edit updatedCard cardEntry) (spinnerWidgetState (Main defaultPage)) cardManagerState state{proxy = proxy'}
        pure $ wrapResponse res

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black
    
    (RestoreCardEvent cardEntry) ->
      (focus "mainView" # liftEffect)
      *>
      do
        ProxyResponse proxy' (Tuple _ card) <- getCardSteps  connectionState  cardsCache            cardEntry             (Main defaultPage) proxyInfo
        let updatedCard                      = restoreCard card
        res                                 <- cardOperationSteps (Edit updatedCard cardEntry) (spinnerWidgetState (Main defaultPage)) cardManagerState state{proxy = proxy'}
        pure $ wrapResponse res

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

  where
    wrapResponse :: Tuple AppState Page -> OperationState
    wrapResponse res = (\page -> WidgetState hiddenOverlayInfo page proxyInfo) <$> res

    spinnerWidgetState :: Page -> String -> WidgetState
    spinnerWidgetState page message = WidgetState (spinnerOverlay message Black) page proxyInfo

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

data CardOperation = Add DataModel.CardVersions.Card.Card | Edit DataModel.CardVersions.Card.Card CardEntry | Delete CardEntry

cardOperationSteps :: CardOperation -> (String -> WidgetState) -> CardManagerState -> AppState -> ExceptT AppError (Widget HTML) (Tuple AppState Page)
cardOperationSteps cardOperation message cardManagerState state@{index: Just index, masterKey: Just masterKey, userInfo: Just userInfo@(UserInfo {userPreferences, donationInfo}), userInfoReferences: Just userInfoReferences, proxy, hash: hashFunc, srpConf, c: Just c, s: Just s, p: Just p, cardsCache, username: Just username, password: Just password, pinEncryptedPassword, enableSync, syncDataWire, donationLevel: Just donationLevel} = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}

  { saveCardOp, deleteCardOp
  , newCardsCache, newIndex, newCardManagerState} <- operationSpecificData
  Tuple newEncryptedIndex    newIndexReference    <- runStep (encryptIndex     newIndex hashFunc         # liftAff)  (message "Compute Encrypted Data")
  newUserInfo                                     <- runStep ((\id -> ( set _userInfo_identifier id >>>
                                                                        set _indexReference newIndexReference
                                                                      ) userInfo
                                                              ) <$> (computeIdentifier                   # liftAff)) (message "Compute Encrypted Data")
  Tuple newEncryptedUserInfo newUserInfoReference <- runStep (encryptUserInfo  newUserInfo hashFunc      # liftAff)  (message "Compute Encrypted Data")
  newUser                                         <- runStep (computeRemoteUserCard srpConf c p s (fst masterKey) =<<
                                                             (computeMasterKey newUserInfoReference p    # liftAff)) (message "Compute Encrypted Data")

  let syncOperations =  ( saveCardOp
                       <> ( Tuple (SaveBlob (_index_reference  # flip view newUserInfo)
                                            (_index_identifier # flip view newIndex)
                                            (newEncryptedIndex # fromArrayBuffer)
                                  )
                                  "Save Index"
                          )
                        : ( Tuple (SaveBlob (_userInfo_reference  # flip view newUserInfoReference)
                                            (_userInfo_identifier # flip view newUserInfo)
                                            (newEncryptedUserInfo # fromArrayBuffer)
                                  )
                                  "Save User Info"
                          )
                        : ( Tuple (SaveUser    newUser)                                             
                                  "Update User Info"
                          )
                        : ( Tuple (DeleteBlob (_userInfo_reference  # flip view userInfoReferences)
                                              (_userInfo_identifier # flip view userInfo)
                                  )
                                  "Delete old Index"
                          )
                        : ( Tuple (DeleteBlob (_index_reference  # flip view userInfo)
                                              (_index_identifier # flip view index)
                                  )
                                  "Delete old User Info"
                          )
                        :   Nil
                       <> deleteCardOp
                        )

  _        <- syncLocalStorage syncDataWire    (syncOperations <#> fst) (message "Sync Data to Local Storage")
  newProxy <- syncBackend      connectionState  syncOperations           message

  pure (Tuple 
        (state  { proxy = newProxy, cardsCache = newCardsCache
                , index = Just newIndex, userInfo = Just newUserInfo, userInfoReferences = Just newUserInfoReference
                , masterKey = Just (unwrap newUser).masterKey
                })
        (Main { index:            newIndex
              , credentials:     {username, password}
              , donationInfo
              , pinExists: isJust pinEncryptedPassword
              , enableSync
              , userPreferences
              , userAreaState:    userAreaInitialState
              , cardManagerState: newCardManagerState
              , donationLevel
              , syncDataWire: Just syncDataWire
              }
        )
      )

  where
    operationSpecificData :: ExceptT AppError (Widget HTML) { saveCardOp :: List (Tuple SyncOperation String), deleteCardOp :: List (Tuple SyncOperation String), newCardsCache :: CardsCache, newIndex :: Index, newCardManagerState :: CardManagerState }
    operationSpecificData = case cardOperation of
      Add newCard -> do
        Tuple newEncryptedCard newCardEntry <- runStep (createCardEntry  hashFuncSHA256 newCard # liftAff) (message "Compute Encrypted Data")
        newIndex                            <- runStep (addToIndex       newCardEntry   index   # liftAff) (message "Compute Encrypted Data")

        pure $ {
          saveCardOp: singleton ( Tuple (SaveBlob (_card_reference  # flip view newCardEntry)
                                                  (_card_identifier # flip view newCardEntry)
                                                  (newEncryptedCard # fromArrayBuffer)
                                        )
                                        "Save Card"
                                )
        , deleteCardOp:         Nil
        , newCardsCache:        insert (view _card_reference newCardEntry) newCard cardsCache
        , newIndex:             newIndex
        , newCardManagerState:  cardManagerState {cardViewState = Card newCard newCardEntry, highlightedEntry = Nothing}
        }
      
      Edit updatedCard oldCardEntry -> do
          Tuple newEncryptedCard newCardEntry <- runStep (createCardEntry hashFuncSHA256 updatedCard # liftAff) (message "Compute Encrypted Data")
          newIndex                            <- runStep (( addToIndex       newCardEntry index >>=
                                                            removeFromIndex oldCardEntry)            # liftAff) (message "Compute Encrypted Data")

          pure $ {
            saveCardOp:   singleton ( Tuple (SaveBlob (_card_reference  # flip view newCardEntry)
                                                      (_card_identifier # flip view newCardEntry)
                                                      (newEncryptedCard # fromArrayBuffer)
                                            )
                                            "Save Card"
                                    )
          , deleteCardOp: singleton ( Tuple (DeleteBlob (_card_reference  # flip view oldCardEntry)
                                                        (_card_identifier # flip view oldCardEntry)
                                            )
                                            "Delete old Card"
                                    )
          , newCardsCache:          ( insert (view _card_reference newCardEntry) updatedCard <<< 
                                      delete (view _card_reference oldCardEntry)
                                    ) cardsCache
          , newIndex:               newIndex
          , newCardManagerState:    cardManagerState {cardViewState = Card updatedCard newCardEntry, highlightedEntry = Nothing}
          }
      
      Delete cardEntry -> do
        newIndex <- runStep ((removeFromIndex cardEntry index) # liftAff) (message "Compute Encrypted Data")

        pure $ {
            saveCardOp:             Nil
          , deleteCardOp: singleton ( Tuple (DeleteBlob (_card_reference  # flip view cardEntry)
                                                        (_card_identifier # flip view cardEntry)
                                            )
                                            "Delete old Card"
                                    )
          , newCardsCache:          (delete (view _card_reference cardEntry)) cardsCache
          , newIndex:               newIndex
          , newCardManagerState:    cardManagerState {cardViewState = NoCard, highlightedEntry = Nothing}
          }
        
cardOperationSteps _ _ _ _ = throwError $ InvalidStateError (CorruptedState "cardOperationStep")
