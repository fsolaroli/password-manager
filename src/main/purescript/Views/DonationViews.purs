module Views.DonationViews where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div, iframe, span, text)
import Concur.React.Props as Props
import Control.Alt (void, ($>), (<#>), (<$))
import Control.Alternative (pure)
import Control.Bind (bind, discard, (=<<))
import Data.CommutativeRing ((*))
import Data.Either (Either, hush)
import Data.Formatter.DateTime (format)
import Data.Function (flip, ($))
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Number (fromString)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Time.Duration (Days(..))
import DataModel.UserVersions.User (DonationInfo)
import DataModel.UserVersions.UserCodecs (iso8601DateFormatter)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Donations (DonationLevel(..), donationLevelClass)
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

donationReminder :: DonationLevel -> Widget HTML DonationPageEvent
donationReminder DonationOk = text ""
donationReminder donationLevel = do
  let donationButton = button [void Props.onClick] [span [] [text "Support Clipperz"]]

  div [Props.classList [Just "donationButton", Just $ donationLevelClass donationLevel]] [donationButton]

  event <-  div [Props.classList [Just "donationButton", Just "overlayOpen", Just $ donationLevelClass donationLevel]] [
              div [Props.className "disableOverlay"] [
                div [Props.className "mask", Props.onClick $> CloseDonationPage] []
              , div [Props.className "dialog"] [
                  donationIFrame =<< (liftEffect $ donationIFrameURL "button/")
                ]
              ]
            , donationButton $> CloseDonationPage
            ]
  case event of
    CloseDonationPage -> donationReminder donationLevel
    _                 -> pure event
    
donationUserArea :: Maybe DonationInfo -> Widget HTML DonationPageEvent
donationUserArea donationInfo = 
  div [Props._id "donationUserArea"] [
    div [Props.className "donationInfo"] $ case donationInfo of
      Just {dateOfLastDonation, nextDonationReminder} -> [
        span [Props.className "dateOfLastDonation"] [text $ format iso8601DateFormatter dateOfLastDonation]
      , span [Props.className "dateOfNextReminder"] [text $ format iso8601DateFormatter nextDonationReminder]
      ]
      Nothing -> [ text "We don't have any donation bound to this account 🥹" ]
  , donationIFrame =<< (liftEffect $ donationIFrameURL "userarea/")
  ]