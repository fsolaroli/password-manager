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
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..))
import DataModel.AppState (AppState, InvalidStateError(..), ProxyInfo, ProxyResponse(..))
import DataModel.FragmentState as Fragment
import DataModel.UserVersions.User (UserInfo(..))
import DataModel.WidgetState (CardFormInput(..), CardViewState(..), Page(..), WidgetState(..))
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Functions.Communication.Users (updateUserInfo)
import Functions.Donations (DonationLevel(..), computeDonationLevel)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, noOperation, handleOperationResult, runStep)
import Record (merge)
import Views.AppView (emptyMainPageWidgetState)
import Views.CardsManagerView (cardManagerInitialState)
import Views.CreateCardView (emptyCardFormData)
import Views.DonationViews (DonationPageEvent(..))
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.UserAreaView (userAreaInitialState)

handleDonationPageEvent :: DonationPageEvent -> AppState -> ProxyInfo -> Fragment.FragmentState -> Widget HTML OperationState

handleDonationPageEvent donationPageEvent state@{username: Just username, password: Just password, index: Just index, userInfo: Just userInfo@(UserInfo {userPreferences, donationInfo}), pinEncryptedPassword, donationLevel: Just donationLevel} proxyInfo fragmentState = do
  let defaultPage = { index
                    , credentials:      {username, password}
                    , donationInfo
                    , pinExists:        isJust pinEncryptedPassword
                    , userPreferences
                    , userAreaState:    userAreaInitialState
                    , cardManagerState: cardManagerInitialState
                    , donationLevel
                    }

  case donationPageEvent of
    UpdateDonationLevel days  ->
      do
        newUserInfo                     <- runStep ((\now -> pure $ UserInfo ((unwrap userInfo) {donationInfo = do
                                                      nextDonationReminder <- adjust days now
                                                      pure {dateOfLastDonation: now, nextDonationReminder}})
                                                    ) =<< liftEffect nowDateTime)                        (WidgetState (spinnerOverlay "Update user info" Black) (Main emptyMainPageWidgetState { index = index, donationLevel = DonationOk }) proxyInfo)
        ProxyResponse proxy stateUpdate <- runStep (updateUserInfo state newUserInfo)                    (WidgetState (spinnerOverlay "Update user info" Black) (Main emptyMainPageWidgetState { index = index, donationLevel = DonationOk }) proxyInfo)
        newDonationLevel                <- runStep (computeDonationLevel index newUserInfo # liftEffect) (WidgetState (spinnerOverlay "Update user info" Black) (Main emptyMainPageWidgetState { index = index, donationLevel = DonationOk }) proxyInfo)
        
        let cardViewState = case fragmentState of
                        Fragment.AddCard card -> CardForm (emptyCardFormData {card = card}) (NewCardFromFragment card)
                        _                     -> NoCard

        pure $ Tuple 
          (merge stateUpdate state {proxy = proxy, donationLevel = Just newDonationLevel})
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