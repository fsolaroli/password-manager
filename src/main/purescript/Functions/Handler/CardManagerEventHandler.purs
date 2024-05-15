module Functions.Handler.CardManagerEventHandler
  ( getCardSteps
  , handleCardManagerEvent
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<#>), (<$>))
import Control.Applicative (pure)
import Control.Bind (bind, (=<<), (>>=))
import Control.Category ((<<<))
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Data.Function (flip, (#), ($), (>>>))
import Data.HexString (fromArrayBuffer)
import Data.Identifier (computeIdentifier)
import Data.Lens (set, view)
import Data.List (List(..), (:))
import Data.Map (delete, insert, lookup)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, uncurry)
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.AppState (AppState, CardsCache)
import DataModel.CardVersions.Card as DataModel.CardVersions.Card
import DataModel.Communication.ConnectionState (ConnectionState)
import DataModel.FragmentState as Fragment
import DataModel.IndexVersions.Index (CardEntry(..), _card_identifier, _card_reference, _index_identifier, addToIndex, reference, removeFromIndex)
import DataModel.Proxy (ProxyInfo, ProxyResponse(..))
import DataModel.SRPVersions.SRP (hashFuncSHA256)
import DataModel.UserVersions.User (UserInfo(..), _indexReference, _index_reference, _userInfo_identifier, _userInfo_reference)
import DataModel.WidgetState (CardFormInput(..), CardManagerState, CardViewState(..), Page(..), WidgetState(..), MainPageWidgetState)
import Effect.Aff.Class (liftAff)
import Functions.Card (appendToTitle, archiveCard, createCardEntry, decryptCard, restoreCard)
import Functions.Communication.Blobs (getBlob)
import Functions.Communication.SyncBackend (syncBackend)
import Functions.Communication.Users (computeMasterKey, computeRemoteUserCard, encryptUserInfo)
import Functions.Handler.DonationEventHandler (handleDonationPageEvent)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, handleOperationResult, noOperation, runStep, syncLocalStorage)
import Functions.Index (encryptIndex)
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
        res <- addCardSteps cardManagerState state card (Main $ defaultPage {cardManagerState = cardManagerState {cardViewState = CardForm (emptyCardFormData {card = card}) (NewCardFromFragment card)}}) proxyInfo
        pure res
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

    (CloneCardEvent cardEntry) ->
      do
        ProxyResponse proxy' (Tuple cardsCache' card) <- getCardSteps connectionState cardsCache cardEntry (Main defaultPage) proxyInfo
        let cloneCard                                  = appendToTitle " - copy" <<< (\(DataModel.CardVersions.Card.Card card') -> DataModel.CardVersions.Card.Card card' {archived = false}) $ card
        res                                           <- addCardSteps cardManagerState state{proxy = proxy', cardsCache = cardsCache'} cloneCard (Main defaultPage) proxyInfo
        pure res

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black
  
    (DeleteCardEvent cardEntry) ->
      do
        deleteCardSteps cardManagerState state cardEntry (Main defaultPage) proxyInfo

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

addCardSteps :: CardManagerState -> AppState -> DataModel.CardVersions.Card.Card -> Page -> ProxyInfo -> ExceptT AppError (Widget HTML) OperationState
addCardSteps cardManagerState state@{index: Just index, masterKey: Just masterKey, userInfo: Just userInfo@(UserInfo {userPreferences, donationInfo}), userInfoReferences: Just userInfoReferences, proxy, hash: hashFunc, srpConf, c: Just c, s: Just s, p: Just p, cardsCache, username: Just username, password: Just password, pinEncryptedPassword, enableSync, syncDataWire, donationLevel: Just donationLevel} newCard page proxyInfo = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}

  Tuple newEncryptedCard     newCardEntry         <- runStep (createCardEntry  hashFuncSHA256 newCard # liftAff)  (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  newIndex                                        <- runStep (addToIndex       newCardEntry index     # liftAff)  (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  Tuple newEncryptedIndex    newIndexReference    <- runStep (encryptIndex     newIndex hashFunc      # liftAff)  (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  newUserInfo                                     <- runStep ((\id -> ( set _userInfo_identifier id >>>
                                                                        set _indexReference newIndexReference
                                                                      ) userInfo
                                                              ) <$> (computeIdentifier                # liftAff)) (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  Tuple newEncryptedUserInfo newUserInfoReference <- runStep (encryptUserInfo  newUserInfo hashFunc   # liftAff)  (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  newUser                                         <- runStep (computeRemoteUserCard srpConf c p s (fst masterKey) =<<
                                                             (computeMasterKey newUserInfoReference p # liftAff)) (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  
  let syncOperations =  ( ( Tuple (SaveBlob (_card_reference  # flip view newCardEntry)
                                            (_card_identifier # flip view newCardEntry)
                                            (newEncryptedCard # fromArrayBuffer)
                                  )
                                  "Save Card"
                          )      
                        : ( Tuple (SaveBlob (_index_reference  # flip view newUserInfo)
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
                        )
  
  _        <- syncLocalStorage syncDataWire (syncOperations <#> fst) (      WidgetState (spinnerOverlay "Sync Data to Local Storage" Black) page proxyInfo)
  newProxy <- syncBackend connectionState syncOperations             (\m -> WidgetState (spinnerOverlay m                            Black) page proxyInfo)

  pure (Tuple 
        (state  { proxy = newProxy, cardsCache = insert (view _card_reference newCardEntry) newCard cardsCache
                , index = Just newIndex, userInfo = Just newUserInfo, userInfoReferences = Just newUserInfoReference
                , masterKey = Just (unwrap newUser).masterKey
                })
        (WidgetState
          hiddenOverlayInfo
          (Main { index:            newIndex
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
addCardSteps _ _ _ _ _ = throwError $ InvalidStateError (CorruptedState "addCardStep")

editCardSteps :: CardManagerState -> AppState -> CardEntry -> DataModel.CardVersions.Card.Card -> Page -> ProxyInfo -> ExceptT AppError (Widget HTML) OperationState
editCardSteps cardManagerState state@{index: Just index, masterKey: Just masterKey, proxy, srpConf, hash: hashFunc, c: Just c, s: Just s, p: Just p, userInfo: Just userInfo@(UserInfo {userPreferences, donationInfo}), userInfoReferences: Just userInfoReferences, cardsCache, username: Just username, password: Just password, pinEncryptedPassword, donationLevel: Just donationLevel, enableSync, syncDataWire} oldCardEntry updatedCard page proxyInfo = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}

  Tuple newEncryptedCard     newCardEntry         <- runStep (createCardEntry hashFuncSHA256 updatedCard # liftAff)  (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  newIndex                                        <- runStep ((addToIndex       newCardEntry index >>=
                                                               removeFromIndex oldCardEntry)             # liftAff)  (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  Tuple newEncryptedIndex    newIndexReference    <- runStep (encryptIndex     newIndex hashFunc         # liftAff)  (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  newUserInfo                                     <- runStep ((\id -> ( set _userInfo_identifier id >>>
                                                                        set _indexReference newIndexReference
                                                                      ) userInfo
                                                              ) <$> (computeIdentifier                   # liftAff)) (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  Tuple newEncryptedUserInfo newUserInfoReference <- runStep (encryptUserInfo  newUserInfo hashFunc      # liftAff)  (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  newUser                                         <- runStep (computeRemoteUserCard srpConf c p s (fst masterKey) =<<
                                                             (computeMasterKey newUserInfoReference p    # liftAff)) (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)

  let syncOperations =  ( ( Tuple (SaveBlob (_card_reference  # flip view newCardEntry)
                                            (_card_identifier # flip view newCardEntry)
                                            (newEncryptedCard # fromArrayBuffer)
                                  )
                                  "Save Card"
                          )      
                        : ( Tuple (SaveBlob (_index_reference  # flip view newUserInfo)
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
                        : ( Tuple (DeleteBlob (_card_reference  # flip view oldCardEntry)
                                              (_card_identifier # flip view oldCardEntry)
                                  )
                                  "Delete old Card"
                          )
                        :   Nil
                        )

  _        <- syncLocalStorage syncDataWire (syncOperations <#> fst) (      WidgetState (spinnerOverlay "Sync Data to Local Storage" Black) page proxyInfo)
  newProxy <- syncBackend connectionState syncOperations             (\m -> WidgetState (spinnerOverlay m                            Black) page proxyInfo)

  pure (Tuple 
        (state  { proxy = newProxy, cardsCache = (insert (view _card_reference newCardEntry) updatedCard <<< 
                                                  delete (view _card_reference oldCardEntry)) cardsCache
                , index = Just newIndex, userInfo = Just newUserInfo, userInfoReferences = Just newUserInfoReference
                , masterKey = Just (unwrap newUser).masterKey
                })
        (WidgetState
          hiddenOverlayInfo
          (Main { index:            newIndex
                , credentials:     {username, password}
                , donationInfo
                , pinExists: isJust pinEncryptedPassword
                , enableSync
                , userPreferences
                , userAreaState:    userAreaInitialState
                , cardManagerState: cardManagerState {cardViewState = Card updatedCard newCardEntry, highlightedEntry = Nothing}
                , donationLevel
                , syncDataWire: Just syncDataWire
                }
          )
          proxyInfo
        )
      )
editCardSteps _ _ _ _ _ _ = throwError $ InvalidStateError (CorruptedState "editCardStep")

deleteCardSteps :: CardManagerState -> AppState -> CardEntry -> Page -> ProxyInfo -> ExceptT AppError (Widget HTML) OperationState
deleteCardSteps cardManagerState state@{index: Just index, masterKey: Just masterKey, proxy, srpConf, hash: hashFunc, c: Just c, s: Just s, p: Just p, userInfo: Just userInfo@(UserInfo {userPreferences, donationInfo}), userInfoReferences: Just userInfoReferences, cardsCache, username: Just username, password: Just password, pinEncryptedPassword, donationLevel: Just donationLevel, enableSync, syncDataWire} cardEntry page proxyInfo = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}

  newIndex                                        <- runStep ((removeFromIndex cardEntry index)          # liftAff)  (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  Tuple newEncryptedIndex    newIndexReference    <- runStep (encryptIndex     newIndex hashFunc         # liftAff)  (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  newUserInfo                                     <- runStep ((\id -> ( set _userInfo_identifier id >>>
                                                                        set _indexReference newIndexReference
                                                                      ) userInfo
                                                              ) <$> (computeIdentifier                   # liftAff)) (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  Tuple newEncryptedUserInfo newUserInfoReference <- runStep (encryptUserInfo  newUserInfo hashFunc      # liftAff)  (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)
  newUser                                         <- runStep (computeRemoteUserCard srpConf c p s (fst masterKey) =<<
                                                             (computeMasterKey newUserInfoReference p    # liftAff)) (WidgetState (spinnerOverlay "Compute Encrypted Data" Black) page proxyInfo)

  let syncOperations =  ( ( Tuple (SaveBlob (_index_reference  # flip view newUserInfo)
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
                        : ( Tuple (DeleteBlob (_card_reference  # flip view cardEntry)
                                              (_card_identifier # flip view cardEntry)
                                  )
                                  "Delete old Card"
                          )
                        :   Nil
                        )

  _        <- syncLocalStorage syncDataWire (syncOperations <#> fst) (      WidgetState (spinnerOverlay "Sync Data to Local Storage" Black) page proxyInfo)
  newProxy <- syncBackend connectionState syncOperations             (\m -> WidgetState (spinnerOverlay m                            Black) page proxyInfo)

  pure (Tuple 
        (state  { proxy = newProxy, cardsCache = (delete (view _card_reference cardEntry)) cardsCache
                , index = Just newIndex, userInfo = Just newUserInfo, userInfoReferences = Just newUserInfoReference
                , masterKey = Just (unwrap newUser).masterKey
                })
        (WidgetState
          hiddenOverlayInfo
          (Main { index:            newIndex
                , credentials:     {username, password}
                , donationInfo
                , pinExists: isJust pinEncryptedPassword
                , enableSync
                , userPreferences
                , userAreaState:    userAreaInitialState
                , cardManagerState: cardManagerState {cardViewState = NoCard, highlightedEntry = Nothing}
                , donationLevel
                , syncDataWire: Just syncDataWire
                }
          )
          proxyInfo
        )
      )
deleteCardSteps _ _ _ _ _ = throwError $ InvalidStateError (CorruptedState "editCardStep")