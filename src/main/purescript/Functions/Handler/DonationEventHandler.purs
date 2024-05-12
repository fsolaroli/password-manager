module Functions.Handler.DonationEventHandler
  ( handleDonationPageEvent
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alternative (pure)
import Control.Bind (bind, (=<<), (>>=))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Data.DateTime (adjust)
import Data.Function ((#), ($))
import Data.HeytingAlgebra (not)
import Data.Lens (view)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.AppState (AppState)
import DataModel.FragmentState as Fragment
import DataModel.Proxy (ProxyInfo, ProxyResponse(..))
import DataModel.UserVersions.User (UserInfo(..), _userInfoReference_reference)
import DataModel.WidgetState (CardFormInput(..), CardViewState(..), Page(..), WidgetState(..))
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Functions.Communication.Users (asMaybe, computeRemoteUserCard, updateUserInfo)
import Functions.Donations (DonationLevel(..), computeDonationLevel)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, handleOperationResult, noOperation, runStep, runWidgetStep)
import OperationalWidgets.Sync (SyncOperation(..), addPendingOperations)
import Record (merge)
import Views.AppView (emptyMainPageWidgetState)
import Views.CardsManagerView (cardManagerInitialState)
import Views.CreateCardView (emptyCardFormData)
import Views.DonationViews (DonationPageEvent(..))
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.UserAreaView (userAreaInitialState)

handleDonationPageEvent :: DonationPageEvent -> AppState -> ProxyInfo -> Fragment.FragmentState -> Widget HTML OperationState

handleDonationPageEvent donationPageEvent state@{c: Just c, p: Just p, s: Just s, srpConf, username: Just username, password: Just password, index: Just index, userInfo: Just userInfo@(UserInfo {userPreferences, donationInfo}), userInfoReferences: Just userInfoReferences, pinEncryptedPassword, enableSync, syncDataWire, donationLevel: Just donationLevel} proxyInfo fragmentState = do
  let defaultPage = { index
                    , credentials:      {username, password}
                    , donationInfo
                    , pinExists:        isJust pinEncryptedPassword
                    , enableSync
                    , userPreferences
                    , userAreaState:    userAreaInitialState
                    , cardManagerState: cardManagerInitialState
                    , donationLevel
                    , syncDataWire: Just syncDataWire
                    }

  case donationPageEvent of
    UpdateDonationLevel days  ->
      do
        let page = Main defaultPage { donationLevel = DonationOk }

        newUserInfo                     <- runStep ((\now -> pure $ UserInfo ((unwrap userInfo) {donationInfo = do
                                                      nextDonationReminder <- adjust days now
                                                      pure {dateOfLastDonation: now, nextDonationReminder}})
                                                    ) =<< liftEffect nowDateTime)                        (WidgetState (spinnerOverlay "Update user info" Black) page proxyInfo)
        ProxyResponse proxy stateUpdate <- runStep (updateUserInfo state newUserInfo)                    (WidgetState (spinnerOverlay "Update user info" Black) page proxyInfo)
        newDonationLevel                <- runStep (computeDonationLevel index newUserInfo # liftEffect) (WidgetState (spinnerOverlay "Update user info" Black) page proxyInfo)
        
        syncOperations <- runStep (if (not enableSync) then (pure Nil) else do
                            user <- computeRemoteUserCard c p s stateUpdate.masterKey srpConf
                            pure  ( (SaveBlob   $ view _userInfoReference_reference stateUpdate.userInfoReferences)
                                  : (SaveUser     user                                                )
                                  : (DeleteBlob $ view _userInfoReference_reference userInfoReferences)
                                  :  Nil
                                  )
                          ) (WidgetState (spinnerOverlay "Compute data to sync" Black) page proxyInfo)
  
        _              <- runWidgetStep (addPendingOperations syncDataWire syncOperations) (WidgetState (spinnerOverlay "Compute data to sync" Black) page proxyInfo)


        let cardViewState = case fragmentState of
                        Fragment.AddCard card -> CardForm (emptyCardFormData {card = card}) (NewCardFromFragment card)
                        _                     -> NoCard

        pure $ Tuple 
          (merge (asMaybe stateUpdate) state {proxy = proxy, donationLevel = Just newDonationLevel})
          (WidgetState 
            hiddenOverlayInfo
            (Main emptyMainPageWidgetState  { index            = index
                                            , cardManagerState = cardManagerInitialState { cardViewState = cardViewState }
                                            , donationLevel    = newDonationLevel
                                            }
            )
            proxyInfo
          )

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

    CloseDonationPage -> noOperation (Tuple state $ WidgetState hiddenOverlayInfo (Main defaultPage) proxyInfo)

handleDonationPageEvent _ state _ _ = do
  throwError $ InvalidStateError (CorruptedState "DonationPage")
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true White