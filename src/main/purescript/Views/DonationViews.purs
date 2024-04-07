module Views.DonationViews where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div, iframe, span, text)
import Concur.React.Props as Props
import Control.Alt ((<#>), (<$))
import Control.Alternative (pure)
import Control.Bind (bind, (=<<))
import Data.CommutativeRing ((*))
import Data.Either (Either, hush)
import Data.Function (flip, ($))
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Number (fromString)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Time.Duration (Days(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Donations (DonationLevel(..))
import Functions.EnvironmentalVariables (donationIFrameURL)
import Functions.Events (getWindowMessage)


data DonationPageEvent = UpdateDonationLevel Days | CloseDonationPage

donationIFrameMessageRegex :: Either String Regex
donationIFrameMessageRegex = regex "^done_(\\d+)$" noFlags


donationIFrame :: String -> Widget HTML DonationPageEvent
donationIFrame destinationPage = do
  res <-  iframe [Props.className "donationIframe", Props.src destinationPage] []
          <>
          (liftAff getWindowMessage)
  pure $ case ((flip match res =<< hush donationIFrameMessageRegex) <#> fromFoldable) of
    Just (_ : (Just monthsString) : Nil) -> case (fromString monthsString) of
      Just months -> UpdateDonationLevel (Days (months * 30.0))
      _           -> CloseDonationPage
    _             -> CloseDonationPage

donationPage :: DonationLevel -> Widget HTML DonationPageEvent
donationPage DonationWarning =
  div [Props.className "donationPage"] [
    CloseDonationPage <$ div [Props.className "closeButton"] [ button [Props.onClick] [span [] [text "remove field"]] ] 
  , donationIFrame =<< (liftEffect $ donationIFrameURL "splash/")
  ]
donationPage _ = text ""
