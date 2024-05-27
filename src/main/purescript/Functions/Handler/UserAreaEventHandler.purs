module Functions.Handler.UserAreaEventHandler
  ( handleUserAreaEvent
  )
  where

import Affjax.ResponseFormat as RF
import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt (map, ($>), (<#>), (<$>))
import Control.Alternative ((*>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Category (identity, (<<<))
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Data.Argonaut.Core (stringify)
import Data.Array (filter, foldM, length, snoc)
import Data.Codec.Argonaut (encode)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), isRight)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Function (flip, (#), ($))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, fromArrayBuffer, hex)
import Data.HeytingAlgebra (not)
import Data.Lens (view)
import Data.List (List(..), fromFoldable, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unit (Unit, unit)
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.AppState (AppState, CardsCache)
import DataModel.CardVersions.Card (Card, CardVersion(..), fromCard)
import DataModel.CardVersions.Card as DataModel.CardVersions.Card
import DataModel.CardVersions.CurrentCardVersions (currentCardCodecVersion)
import DataModel.Communication.ConnectionState (ConnectionState)
import DataModel.Credentials (emptyCredentials)
import DataModel.FragmentState as Fragment
import DataModel.IndexVersions.Index (CardEntry(..), CardReference(..), Index(..), _card_reference, _index_identifier, addToIndex)
import DataModel.Proxy (DataOnLocalStorage(..), DynamicProxy(..), Proxy(..), ProxyInfo, ProxyResponse(..), defaultOnlineProxy, discardResult)
import DataModel.UserVersions.User (IndexReference(..), UserInfo(..), _index_reference, _userInfo_identifier, _userInfo_reference)
import DataModel.WidgetState (CardManagerState, CardViewState(..), ImportStep(..), LoginType(..), Page(..), UserAreaPage(..), UserAreaState, WidgetState(..), MainPageWidgetState)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Card (addTag)
import Functions.Communication.Backend (genericRequest)
import Functions.Communication.Blobs (deleteBlob, getBlob)
import Functions.Communication.Cards (deleteCard, getCard, postCard)
import Functions.Communication.Users (asMaybe, computeRemoteUserCard, deleteUserCard, deleteUserInfo, updateUserPreferences)
import Functions.DeviceSync (computeDeleteOperations, computeSyncOperations, updateSyncPreference)
import Functions.Events (focus)
import Functions.Export (BlobsList, appendCardsDataInPlace, getBasicHTML, prepareUnencryptedExport, prepareHTMLBlob)
import Functions.Handler.DonationEventHandler (handleDonationPageEvent)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, handleOperationResult, noOperation, runStep, runWidgetStep)
import Functions.Import (ImportVersion(..), decodeImport, parseImport, readFile)
import Functions.Index (updateIndex)
import Functions.Pin (deleteCredentials, makeKey, saveCredentials)
import Functions.State (resetState)
import Functions.Time (formatDateTimeToDate, getCurrentDateTime)
import Functions.Timer (activateTimer, stopTimer)
import Functions.User (changeUserPassword)
import OperationalWidgets.Sync (SyncOperation(..), addPendingOperation, updateConnectionState)
import Record (merge)
import Views.DonationViews as DonationEvent
import Views.ExportView (ExportEvent(..))
import Views.LoginFormView (emptyLoginFormData)
import Views.OverlayView (OverlayColor(..), OverlayStatus(..), hiddenOverlayInfo, spinnerOverlay)
import Views.SetPinView (PinEvent(..))
import Views.UserAreaView (UserAreaEvent(..), userAreaInitialState)
import Web.DownloadJs (download)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)

handleUserAreaEvent :: UserAreaEvent -> CardManagerState -> UserAreaState -> AppState -> ProxyInfo -> Fragment.FragmentState -> Widget HTML OperationState


handleUserAreaEvent userAreaEvent cardManagerState userAreaState state@{proxy, srpConf, hash: hashFunc, cardsCache, username: Just username, password: Just password, index: Just index, userInfo: Just userInfo@(UserInfo {indexReference: IndexReference { reference: indexRef}, userPreferences, donationInfo}), userInfoReferences: Just userInfoReferences, c: Just c, p: Just p, s: Just s, masterKey: Just masterKey, pinEncryptedPassword, enableSync, donationLevel: Just donationLevel, syncDataWire} proxyInfo f = do
  let defaultPage = { index
                    , credentials:      {username, password}
                    , donationInfo
                    , pinExists:        isJust pinEncryptedPassword
                    , enableSync
                    , userPreferences
                    , userAreaState
                    , cardManagerState
                    , donationLevel
                    , syncDataWire: Just syncDataWire
                    }

  case userAreaEvent of
    (CloseUserAreaEvent) -> 
      (focus "indexView" # liftEffect)
      *> 
      noOperation (Tuple 
                  state
                  (WidgetState
                    hiddenOverlayInfo
                    (Main defaultPage {userAreaState = userAreaState {showUserArea = false, userAreaOpenPage = None}})
                    proxyInfo
                  )
                )

    (OpenUserAreaPage userAreaPage) -> 
      updateUserAreaState defaultPage userAreaState {userAreaOpenPage = userAreaPage}
    
    (ChangeUserAreaSubmenu userAreaSubmenu) ->
      updateUserAreaState defaultPage userAreaState {userAreaSubmenus = userAreaSubmenu}
    
    (UpdateDonationLevel days) -> handleDonationPageEvent (DonationEvent.UpdateDonationLevel days) state proxyInfo f

    (UpdateUserPreferencesEvent newUserPreferences) ->
      let page = Main defaultPage { userPreferences = newUserPreferences }
      in do
        ProxyResponse proxy' stateUpdateInfo <- runStep (updateUserPreferences state newUserPreferences) (WidgetState (spinnerOverlay "Update user preferences" White) page proxyInfo)
        
        liftEffect $ stopTimer
        case (unwrap newUserPreferences).automaticLock of
          Left  _ -> pure unit
          Right n -> liftEffect $ activateTimer n

        syncOperations <- runStep (if (not enableSync) then (pure Nil) else do
                            user <- computeRemoteUserCard srpConf c p s (fst masterKey) stateUpdateInfo.masterKey
                            pure  ( (SaveBlobFromRef   $ view _index_reference      stateUpdateInfo.userInfo          )
                                  : (SaveBlobFromRef   $ view _userInfo_reference stateUpdateInfo.userInfoReferences)
                                  : (SaveUser     user                                                )
                                  : (DeleteBlob (view _userInfo_reference userInfoReferences) (view _userInfo_identifier userInfo))
                                  : (DeleteBlob (view _index_reference      userInfo        ) (view _index_identifier index))
                                  :  Nil
                                  )
                          ) (WidgetState (spinnerOverlay "Compute data to sync" White) (Main defaultPage) proxyInfo)
  
        _              <- runWidgetStep (addPendingOperation syncDataWire syncOperations) (WidgetState (spinnerOverlay "Compute data to sync" White) (Main defaultPage) proxyInfo)

        pure (Tuple 
          (merge (asMaybe stateUpdateInfo) state {proxy = proxy'})
          (WidgetState hiddenOverlayInfo page proxyInfo)
        )
            
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White   

    (ChangePasswordEvent newPassword) ->
      let page      = Main defaultPage { credentials = {username, password: newPassword} }
          errorPage = Main defaultPage
      in do
        ProxyResponse proxy' userUpdateInfo <- runStep (changeUserPassword state newPassword) (WidgetState (spinnerOverlay "Update password" White) page proxyInfo)
        
        syncOperations <- runStep (if (not enableSync) then (pure Nil) else do
                            user <- computeRemoteUserCard srpConf userUpdateInfo.c userUpdateInfo.p userUpdateInfo.s (fst masterKey) userUpdateInfo.masterKey
                            pure $ (SaveUser user) : (DeleteUser c) : Nil
                          ) (WidgetState (spinnerOverlay "Compute data to sync" White) page proxyInfo)
  
        _              <- runWidgetStep (addPendingOperation  syncDataWire syncOperations) (WidgetState (spinnerOverlay "Compute data to sync" White) page proxyInfo)
        _              <- runWidgetStep (updateConnectionState syncDataWire { c: userUpdateInfo.c
                                                                            , p: userUpdateInfo.p
                                                                            , srpConf, hashFunc
                                                                            , proxy: DynamicProxy defaultOnlineProxy
                                                                            })              (WidgetState (spinnerOverlay "Compute data to sync" White) page proxyInfo)
        _              <- runStep       (   (updateSyncPreference                c false      # liftEffect)
                                         *> (updateSyncPreference userUpdateInfo.c enableSync # liftEffect)
                                        )                                                   (WidgetState (spinnerOverlay "Update sync preferences" White) page proxyInfo)

        pure (Tuple 
          (state {proxy = proxy', c = Just userUpdateInfo.c, p = Just userUpdateInfo.p, s = Just userUpdateInfo.s, masterKey = Just userUpdateInfo.masterKey, password = Just newPassword})
          (WidgetState hiddenOverlayInfo page proxyInfo)
        )

      # runExceptT
      >>= handleOperationResult state errorPage true White
    
    (SetPinEvent pinAction) ->
      let page = Main defaultPage { pinExists = case pinAction of
                                                  Reset    -> false
                                                  SetPin _ -> true
                                  }
      in do
        storage               <- liftEffect $ window >>= localStorage
        pinEncryptedPassword' <- runStep (case pinAction of
                                          Reset      -> (liftEffect $ deleteCredentials storage)  $> Nothing
                                          SetPin pin -> (saveCredentials state pin storage)      <#> Just
                                        ) (WidgetState
                                            (spinnerOverlay (case pinAction of
                                                              Reset    -> "Reset PIN"
                                                              SetPin _ -> "Set PIN") 
                                                            White)
                                            page
                                            proxyInfo
                                          )
        pure (Tuple 
                (state {pinEncryptedPassword = pinEncryptedPassword'})
                (WidgetState
                  hiddenOverlayInfo
                  page
                  proxyInfo
                )
              )

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White

    (UpdateSyncPreference enableSync') -> do
      _              <- runStep       (updateSyncPreference c enableSync' # liftEffect ) (WidgetState {status: Spinner, color: Black, message: "Compute data to sync"} (Main defaultPage) proxyInfo)
      let updatedState = state {enableSync = enableSync'}
      syncOperations <- if enableSync'
                     then
                        runStep       (computeSyncOperations   updatedState            ) (WidgetState {status: Spinner, color: Black, message: "Compute data to sync"} (Main defaultPage) proxyInfo)
                     else 
                        runStep       (computeDeleteOperations updatedState            ) (WidgetState {status: Spinner, color: Black, message: "Compute data to sync"} (Main defaultPage) proxyInfo)
      _              <- runWidgetStep (addPendingOperation syncDataWire syncOperations) (WidgetState {status: Spinner, color: Black, message: "Compute data to sync"} (Main defaultPage) proxyInfo)
      pure (Tuple 
              updatedState
              (WidgetState
                hiddenOverlayInfo
                (Main defaultPage {enableSync = enableSync'})
                proxyInfo
              )
            )

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White

    (DeleteAccountEvent) ->
      let page = Main defaultPage
      in do
        let connectionState = {proxy, hashFunc, srpConf, c, p}
        ProxyResponse proxy'    _ <-                deleteCardsSteps connectionState                   cardsCache       index                                                                      page proxyInfo
        ProxyResponse proxy''   _ <- runStep       (deleteBlob       connectionState{proxy = proxy'  } indexRef (unwrap index).identifier) (WidgetState (spinnerOverlay "Delete Index"     White)  page proxyInfo)
        ProxyResponse proxy'''  _ <- runStep       (deleteUserInfo   connectionState{proxy = proxy'' } userInfo userInfoReferences       ) (WidgetState (spinnerOverlay "Delete User Info" White)  page proxyInfo)
        ProxyResponse proxy'''' _ <- runStep       (deleteUserCard   connectionState{proxy = proxy'''} c                                 ) (WidgetState (spinnerOverlay "Delete User Card" White)  page proxyInfo)
        _                         <- runStep       (liftEffect $ window >>= localStorage >>= deleteCredentials                           ) (WidgetState (spinnerOverlay "Delete local data" White) page proxyInfo)    
        syncOperations            <- runStep       (computeDeleteOperations state                                                        ) (WidgetState (spinnerOverlay "Delete local data" White) page proxyInfo)
        _                         <- runStep       (updateSyncPreference c false # liftEffect                                            ) (WidgetState (spinnerOverlay "Delete local data" White) page proxyInfo)
        _                         <- runWidgetStep (addPendingOperation syncDataWire syncOperations                                     ) (WidgetState (spinnerOverlay "Delete local data" White) page proxyInfo)

        
        pure $ Tuple 
                (resetState state {proxy = proxy''''})
                (WidgetState
                  hiddenOverlayInfo
                  (Login emptyLoginFormData { credentials = emptyCredentials
                                            , loginType   = CredentialLogin
                                            }
                  )
                  proxyInfo
                ) 
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White
   
    (ImportCardsEvent importState) ->
      let page = Main defaultPage { userAreaState = userAreaState {importState = importState} }
      in case importState.step of
        Upload    ->
          do
            result    <- runStep  (case importState.content of
                                    Left  file   -> readFile file >>= parseImport
                                    Right string -> flip decodeImport string <$> [Epsilon CardVersion_1, Delta] 
                                                    # (foldM  (\finalResult singleResult -> do
                                                                if (isRight finalResult) 
                                                                then pure $ finalResult
                                                                else do
                                                                  res <- runExceptT singleResult
                                                                  if (isRight res)
                                                                  then pure $ res
                                                                  else pure $ finalResult
                                                              )
                                                              (Left $ ImportError "Invalid input: unable to decode data")
                                                      )
                                                    # ExceptT
                                  ) (WidgetState (spinnerOverlay "Parse Data" White) page proxyInfo)

            currentDate <- runStep ((((<>) "Import_") <<< formatDateTimeToDate) <$> (liftEffect getCurrentDateTime)) (WidgetState (spinnerOverlay "Get current date" White) page proxyInfo)

            pure $ Tuple 
                    state $
                    WidgetState
                      hiddenOverlayInfo
                      (Main defaultPage  { userAreaState = userAreaState 
                                                            { importState = importState
                                                                              { content = Right $ stringify $ encode (CA.array currentCardCodecVersion) $ fromCard <$> result
                                                                              , step = Selection, tag = Tuple true currentDate, selection = result <#> (\card@(DataModel.CardVersions.Card.Card r) -> Tuple (not r.archived) card)
                                                                              }
                                                            }
                                        }
                      ) 
                      proxyInfo

          # runExceptT
          >>= handleOperationResult state page true White
      
        Selection ->
          noOperation $ Tuple 
                        state $
                        WidgetState
                          hiddenOverlayInfo
                          (Main defaultPage { userAreaState = userAreaState {importState = importState {step = Confirm}} })
                          proxyInfo
        
        Confirm   ->
          do
            let connectionState = {proxy, hashFunc, srpConf, c, p}
            let cardToImport                                  = filter fst importState.selection <#> snd # (if fst importState.tag
                                                                                                            then (map $ addTag (snd importState.tag))
                                                                                                            else identity
                                                                                                          )
            let nToImport                                     = length cardToImport
            ProxyResponse proxy' (Tuple cardsCache' entries) <- foldWithIndexM (\i (ProxyResponse proxy' (Tuple cardsCache' entries)) card -> do
              ProxyResponse proxy'' (Tuple cardsCache'' newCardEntry) <- runStep (postCard connectionState{proxy = proxy'} cardsCache' card) (WidgetState (spinnerOverlay ("Import card " <> show i <> " of " <> show nToImport) White) page proxyInfo)
              pure $ ProxyResponse proxy'' (Tuple cardsCache'' (snoc entries newCardEntry))
            ) (ProxyResponse proxy (Tuple cardsCache [])) cardToImport
            updatedIndex                                     <- runStep (foldM (flip addToIndex) index entries # liftAff)   (WidgetState (spinnerOverlay "Update index" White) page proxyInfo)
            ProxyResponse proxy'' stateUpdateInfo            <- runStep (updateIndex (state {proxy = proxy'}) updatedIndex) (WidgetState (spinnerOverlay "Update index" White) page proxyInfo)

            syncOperations <- runStep (if (not enableSync) then (pure Nil) else do
                                user <- computeRemoteUserCard srpConf c p s (fst masterKey) stateUpdateInfo.masterKey
                                pure $  ( (SaveBlobFromRef   $ view _index_reference      stateUpdateInfo.userInfo          )
                                        : (SaveBlobFromRef   $ view _userInfo_reference stateUpdateInfo.userInfoReferences)
                                        : (SaveUser     user                                                )
                                        : (DeleteBlob (view _userInfo_reference userInfoReferences) (view _userInfo_identifier userInfo))
                                        : (DeleteBlob (view _index_reference      userInfo          ) (view _index_identifier index))
                                        :  Nil
                                        ) <> ((SaveBlobFromRef <<< view _card_reference) <$> (entries # fromFoldable))
                              ) (WidgetState (spinnerOverlay "Compute data to sync" White) (Main defaultPage) proxyInfo)
      
            _              <- runWidgetStep (addPendingOperation syncDataWire syncOperations) (WidgetState (spinnerOverlay "Compute data to sync" White) (Main defaultPage) proxyInfo)


            pure (Tuple 
              (merge (asMaybe stateUpdateInfo) state {proxy = proxy'', cardsCache = cardsCache', index = Just updatedIndex}
              )
              (WidgetState
                hiddenOverlayInfo
                (Main defaultPage { index            = updatedIndex
                                  , userAreaState    = userAreaInitialState
                                  , cardManagerState = cardManagerState {cardViewState = NoCard, highlightedEntry = Nothing}
                                  }
                )
                proxyInfo
              )
          )
      
          # runExceptT
          >>= handleOperationResult state page true White  

    (ExportEvent OfflineCopy) ->
      let page = Main defaultPage
      in do
        let connectionState         =         {proxy, hashFunc, srpConf, c, p}
        let references              =         userInfoReferences.reference : indexRef : ((\(CardEntry {cardReference: CardReference {reference}}) -> reference) <$> (unwrap index).entries)

        ProxyResponse proxy' blobs <-          downloadBlobsSteps references connectionState page proxyInfo
        remoteUserCard             <- runStep (computeRemoteUserCard srpConf c p s (hex "") masterKey)                                                                                  (WidgetState (spinnerOverlay "Compute user card" White) page proxyInfo)
        ProxyResponse proxy'' doc  <- runStep (getBasicHTML connectionState{proxy = proxy'})                                                                 (WidgetState (spinnerOverlay "Download html"     White) page proxyInfo)
        documentToDownload         <- runStep (appendCardsDataInPlace doc blobs remoteUserCard >>= (liftEffect <<< prepareHTMLBlob))                         (WidgetState (spinnerOverlay "Create document"   White) page proxyInfo)
        date                       <- runStep (liftEffect $ formatDateTimeToDate <$> getCurrentDateTime)                                                     (WidgetState (spinnerOverlay ""                  White) page proxyInfo)
        _                          <- runStep (liftEffect $ download documentToDownload (date <> "_Clipperz_Offline" <> ".html") "application/octet-stream") (WidgetState (spinnerOverlay "Download document" White) page proxyInfo)
        pure $ Tuple state {proxy = proxy''} (WidgetState hiddenOverlayInfo page proxyInfo)
      
      # runExceptT
      >>= handleOperationResult state page true White

    (ExportEvent UnencryptedCopy) ->
      let page = Main defaultPage
      in do
        let connectionState = {proxy, hashFunc, srpConf, c, p}
        ProxyResponse proxy' (Tuple cardsCache' cardList) <-                       downloadCardsSteps (unwrap index).entries cardsCache connectionState page proxyInfo
        doc                                               <- runStep (liftEffect $ prepareUnencryptedExport cardList)                                             (WidgetState (spinnerOverlay "Create document"   White) page proxyInfo)
        date                                              <- runStep (liftEffect $ formatDateTimeToDate <$> getCurrentDateTime)                                   (WidgetState (spinnerOverlay ""                  White) page proxyInfo)
        _                                                 <- runStep (liftEffect $ download doc (date <> "_Clipperz_Export_" <> username <> ".html") "text/html") (WidgetState (spinnerOverlay "Download document" White) page proxyInfo)
                                  
        pure $ Tuple state{proxy = proxy', cardsCache = cardsCache'} (WidgetState hiddenOverlayInfo page proxyInfo)
      # runExceptT
      >>= handleOperationResult state page true White

    (LockEvent) ->
      let page = Main defaultPage
      in
        logoutSteps state "Lock" page proxyInfo
        # runExceptT
        >>= handleOperationResult state defaultErrorPage true White
  
    (LogoutEvent) ->
      let page = Main defaultPage
      in 
        do
          username_ <- runStep (liftEffect $ window >>= localStorage >>= getItem (makeKey "user")) (WidgetState (spinnerOverlay "Logout" White) page proxyInfo)
          logoutSteps (state {username = username_}) "Logout" page proxyInfo
        # runExceptT
        >>= handleOperationResult state defaultErrorPage true White

  where
    updateUserAreaState :: MainPageWidgetState -> UserAreaState -> Widget HTML OperationState
    updateUserAreaState mainPageState userAreaState' = 
      noOperation (Tuple 
                    state
                    (WidgetState
                      hiddenOverlayInfo
                      (Main mainPageState { userAreaState = userAreaState' }
                      )
                      proxyInfo
                    )
                  )  

handleUserAreaEvent _ _ _ state _ _ = do
  throwError $ InvalidStateError (CorruptedState "userAreaEvent")
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true White

-- ===================================================================================================

downloadCardsSteps :: List CardEntry -> CardsCache -> ConnectionState -> Page -> ProxyInfo -> ExceptT AppError (Widget HTML) (ProxyResponse (Tuple CardsCache (List Card)))
downloadCardsSteps cardEntryList cardsCache connectionState page proxyInfo = 
  foldWithIndexM (\i (ProxyResponse proxy' (Tuple cardsCache' cards)) cardEntry -> do
    ProxyResponse proxy'' (Tuple cardsCache'' card) <- runStep (getCard connectionState{proxy = proxy'} cardsCache' cardEntry) (WidgetState (spinnerOverlay ("Download card " <> show i <> " of " <> show nToDownload) White) page proxyInfo)
    pure $ ProxyResponse proxy'' (Tuple cardsCache'' (List.snoc cards card))
  ) (ProxyResponse connectionState.proxy (Tuple cardsCache Nil)) cardEntryList
  where
    nToDownload = List.length cardEntryList

downloadBlobsSteps :: List HexString -> ConnectionState -> Page -> ProxyInfo -> ExceptT AppError (Widget HTML) (ProxyResponse BlobsList)
downloadBlobsSteps referenceList connectionState page proxyInfo = 
  foldWithIndexM (\i (ProxyResponse proxy' blobs) reference -> do
    ProxyResponse proxy'' arrayBuffer <- runStep (getBlob connectionState{proxy = proxy'} reference) (WidgetState (spinnerOverlay ("Download blob " <> show i <> " of " <> show nToDownload) White) page proxyInfo)
    pure $ ProxyResponse proxy'' (List.snoc blobs (Tuple reference (fromArrayBuffer arrayBuffer)))
  ) (ProxyResponse connectionState.proxy Nil) referenceList
  where
    nToDownload = List.length referenceList

logoutSteps :: AppState -> String -> Page -> ProxyInfo -> ExceptT AppError (Widget HTML) OperationState
logoutSteps state@{username, hash: hashFunc, proxy, srpConf} message page proxyInfo =
  do
    proxy' <- case proxy of
      DynamicProxy (OfflineProxy _ _) -> pure $ DynamicProxy $ OfflineProxy Nothing NoData
      _ -> do 
        let connectionState = {proxy, hashFunc, srpConf, c: hex "", p: hex ""}
        _ <- runStep (genericRequest connectionState "logout" POST Nothing RF.ignore) (WidgetState (spinnerOverlay message White) page proxyInfo)
        pure $ DynamicProxy defaultOnlineProxy

    passphrase <- runStep (liftEffect $ window >>= localStorage >>= getItem (makeKey "passphrase")) (WidgetState (spinnerOverlay message White) page proxyInfo)
    
    pure $ Tuple 
            ((resetState state) {username = username, pinEncryptedPassword = hex <$> passphrase, proxy = proxy'})
            (WidgetState
              hiddenOverlayInfo
              (Login emptyLoginFormData { credentials = emptyCredentials {username = fromMaybe "" username}
                                        , loginType   = if isNothing passphrase then CredentialLogin else PinLogin
                                        }
              )
              proxyInfo
            ) 

deleteCardsSteps :: ConnectionState -> CardsCache -> Index -> Page -> ProxyInfo -> ExceptT AppError (Widget HTML) (ProxyResponse Unit)
deleteCardsSteps connectionState cardsCache (Index {entries}) page proxyInfo =
  foldWithIndexM (\i (ProxyResponse proxy' _) cardEntry -> do
    discardResult <$> runStep (deleteCard connectionState{proxy = proxy'} cardsCache (unwrap cardEntry).cardReference) (WidgetState (spinnerOverlay ("Delete card " <> show i <> " of " <> show nToDelete) White) page proxyInfo)
  ) (ProxyResponse connectionState.proxy unit) entries

  where
    nToDelete = List.length entries