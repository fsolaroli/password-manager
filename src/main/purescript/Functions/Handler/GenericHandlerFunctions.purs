module Functions.Handler.GenericHandlerFunctions where

import Concur.Core (Widget, liftWidget)
import Concur.React (HTML, affAction)
import Control.Alt ((<|>))
import Control.Alternative ((*>), (<*))
import Control.Applicative (pure)
import Control.Bind (discard, (=<<))
import Control.Category ((<<<))
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either, either)
import Data.Function ((#), ($))
import Data.Functor ((<$))
import Data.Int (toNumber)
import Data.Show (show)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppError (AppError)
import DataModel.AppState (AppState)
import DataModel.WidgetState (Page(..), WidgetState(..))
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Functions.State (getProxyInfoFromProxy)
import Unsafe.Coerce (unsafeCoerce)
import Views.AppView (appView)
import Views.LoginFormView (emptyLoginFormData)
import Views.OverlayView (OverlayColor, OverlayStatus(..))

type OperationState = Tuple AppState WidgetState

foreign import _operationDelay :: Unit -> Effect Number

operationDelay :: Effect Number 
operationDelay = _operationDelay unit

runStep :: forall a. ExceptT AppError Aff a -> WidgetState -> ExceptT AppError (Widget HTML) a
runStep       step widgetState = ExceptT    $ ((step # runExceptT # affAction) <* ((affAction <<< delay <<< Milliseconds) =<< (liftEffect operationDelay))) <|> (defaultView widgetState)

runWidgetStep :: forall a. Widget HTML a -> WidgetState -> ExceptT AppError (Widget HTML) a
runWidgetStep step widgetState = liftWidget $ ( step                           <* ((affAction <<< delay <<< Milliseconds) =<< (liftEffect operationDelay))) <|> (defaultView widgetState)

defaultView :: forall a. WidgetState -> Widget HTML a
defaultView widgetState = (unsafeCoerce unit <$ appView widgetState)

defaultErrorPage :: Page
defaultErrorPage = Login emptyLoginFormData

handleOperationResult :: AppState -> Page -> Boolean -> OverlayColor -> Either AppError OperationState -> Widget HTML OperationState
handleOperationResult state@{proxy} page showDone color =
  either
    manageError
    (\res@(Tuple _ (WidgetState _ page' proxyInfo')) ->
      if   showDone
      then delayOperation 500 (WidgetState { status: Done, color, message: "" } page' proxyInfo') *> pure res
      else pure res
    )

                                                
  where
    manageError :: AppError -> Widget HTML OperationState
    manageError error = 
      case error of
        -- _ -> ErrorPage --TODO
        err -> do
          liftEffect $ log $ show err
          delayOperation 500 (WidgetState { status: Failed, color, message: "error" } page (getProxyInfoFromProxy proxy))
          pure $ Tuple state (WidgetState { status: Hidden, color, message: ""      } page (getProxyInfoFromProxy proxy))

delayOperation :: Int -> WidgetState -> Widget HTML Unit
delayOperation time widgetState = ((affAction $ delay (Milliseconds $ toNumber time)) <|> (unit <$ appView widgetState))

noOperation :: OperationState -> Widget HTML OperationState 
noOperation operationState@(Tuple _ widgetState) = (pure operationState) <|> (unsafeCoerce unit <$ appView widgetState)
