module OperationalWidgets.App ( app ) where

import Concur.Core (Widget)
import Concur.React (HTML, affAction)
import Control.Alt ((<|>))
import Control.Alternative (pure, (*>))
import Control.Bind (bind, (=<<), (>>=))
import Data.Function (($))
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppState, ProxyInfo)
import DataModel.FragmentState as Fragment
import DataModel.WidgetState (Page(..), WidgetState(..))
import Effect.Class (liftEffect)
import Functions.Events (online)
import Functions.Handler.CardManagerEventHandler (handleCardManagerEvent)
import Functions.Handler.DonationEventHandler (handleDonationPageEvent)
import Functions.Handler.GenericHandlerFunctions (OperationState)
import Functions.Handler.LoginPageEventHandler (handleLoginPageEvent)
import Functions.Handler.SignupPageEventHandler (getLoginFormData, handleSignupPageEvent)
import Functions.Handler.UserAreaEventHandler (handleUserAreaEvent)
import Functions.State (computeProxy, getProxyInfoFromProxy)
import Views.AppView (PageEvent(..), appView)
import Views.LoginFormView (LoginPageEvent(..))
import Views.OverlayView (hiddenOverlayInfo)
import Views.SignupFormView (emptyDataForm)

app :: forall a. AppState -> Fragment.FragmentState -> Widget HTML a
app appState@{proxy} fragmentState = case fragmentState of
    Fragment.Login cred   -> appWithInitialOperation        appState                                                                               (LoginPageEvent $ LoginEvent cred)
    Fragment.Registration -> appLoop                 (Tuple appState (WidgetState hiddenOverlayInfo (Signup  emptyDataForm)             proxyInfo))
    _                     -> appLoop                 (Tuple appState (WidgetState hiddenOverlayInfo (Login $ getLoginFormData appState) proxyInfo))

  where
    proxyInfo :: ProxyInfo
    proxyInfo = getProxyInfoFromProxy proxy

    appWithInitialOperation :: AppState -> PageEvent -> Widget HTML a
    appWithInitialOperation state event = do
      appLoop =<< executeOperation event state proxyInfo fragmentState

    appLoop :: OperationState -> Widget HTML a
    appLoop operationState@(Tuple state widgetState@(WidgetState _ _ proxyInfo')) = do
      ( ( do
          resultEvent <- appView widgetState
          executeOperation resultEvent state proxyInfo' fragmentState
        )
        <|>
          updateProxy operationState
      )
      >>= appLoop

executeOperation :: PageEvent -> AppState -> ProxyInfo -> Fragment.FragmentState -> Widget HTML OperationState
executeOperation (SignupPageEvent          event)      = handleSignupPageEvent   event
executeOperation (LoginPageEvent           event)      = handleLoginPageEvent    event
executeOperation (MainPageCardManagerEvent event s)    = handleCardManagerEvent  event s
executeOperation (MainPageUserAreaEvent    event s s') = handleUserAreaEvent     event s s'
executeOperation (DonationPageEvent        event)      = handleDonationPageEvent event

updateProxy :: OperationState -> Widget HTML OperationState
updateProxy (Tuple state' (WidgetState overlayInfo' page' _)) =
  affAction online *> do
    newProxy <- liftEffect $ computeProxy
    pure $ (Tuple state' {proxy = newProxy} (WidgetState overlayInfo' page' (getProxyInfoFromProxy newProxy)))