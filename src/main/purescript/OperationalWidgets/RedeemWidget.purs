module OperationalWidgets.RedeemWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, p, text)
import Concur.React.Props as Props
import Control.Alt ((<$), (<|>))
import Control.Bind (bind, (>>=))
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.HexString (hex, toArrayBuffer)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import DataModel.AppState (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Functions.Communication.StatelessBackend (ConnectionState)
import Functions.Communication.StatelessOneTimeShare (decryptSecret, redeem)
import Functions.EnvironmentalVariables (currentCommit)
import Views.Components (Enabled(..))
import Views.OverlayView (OverlayStatus(..), overlay)
import Views.RedeemView (redeemView, redeemedView)
import Web.HTML (window)
import Web.HTML.History (DocumentTitle(..), URL(..), replaceState)
import Web.HTML.Location (pathname)
import Web.HTML.Window (history, location)

redeemWidget :: ConnectionState -> String -> String -> Widget HTML Unit
redeemWidget connectionState id cryptedKey = do
  version <- liftEffect currentCommit
  do
    pin <- redeemView (Enabled true)
    eitherSecret <- ( liftAff $ runExceptT $ do
                        (Tuple secretVersion enryptedSecret) <- redeem connectionState id
                        decryptSecret secretVersion pin (toArrayBuffer $ hex cryptedKey) enryptedSecret
                    )
                    <|>
                    ( overlay { status: Spinner, message: "loading" } )
                    <|>
                    ( Right "" <$ redeemView (Enabled false) ) 
    pathName <- liftEffect $ window >>= location >>= pathname
    _ <-        liftEffect $ window >>= history >>= replaceState (unsafeToForeign {}) (DocumentTitle "") (URL pathName)
    case eitherSecret of
      Right secret ->
        redeemedView secret
      Left err -> case err of
        ProtocolError (ResponseError   404) -> div [Props.className "warning"] [text $ "Secret already redeemed"]
        ProtocolError (ResponseError   410) -> div [Props.className "warning"] [text $ "Secret expired"]
        ProtocolError (ResponseError   _  ) -> div [Props.className "error"]   [text $ "Error retrieving document"]
        ProtocolError (IllegalResponse _  ) -> div [Props.className "error"]   [text $ "Error retrieving document"]
        ProtocolError (CryptoError     _  ) -> div [Props.className "error"]   [text $ "Error decrypting document"]
        e                                   -> div [Props.className "error"]   [text $ "Unhandled error [" <> show e <> "]"]
    <> p [Props.className "version"] [text version]