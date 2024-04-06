module Views.DonationViews where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div, iframe, int, span, strong, text)
import Concur.React.Props as Props
import Control.Alt (void, ($>), (<#>), (<$))
import Control.Alternative (pure)
import Control.Bind (bind, discard, (=<<), (>>=))
import Data.CommutativeRing ((*))
import Data.DateTime (DateTime, diff)
import Data.Either (Either(..), hush)
import Data.EuclideanRing ((-))
import Data.Formatter.DateTime (format)
import Data.Function (flip, (#), ($))
import Data.Int (ceil, floor)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Number (fromString)
import Data.Ord ((>))
import Data.Semigroup (append)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Time.Duration (Days(..))
import DataModel.UserVersions.User (DonationInfo)
import DataModel.UserVersions.UserCodecs (iso8601DateFormatter)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Functions.Donations (DonationLevel(..), computeNextDonationReminderDate, donationLevelClass)
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
  , donationIFrame =<< (liftEffect donationIFrameURL <#> (flip append "splash/"))
  ]
donationPage _ = text ""

donationReminder :: DonationLevel -> Widget HTML DonationPageEvent
donationReminder DonationOk = text ""
donationReminder donationLevel = do
  div [Props.classList [Just "donationButton", Just $ donationLevelClass donationLevel]] [
    button [void Props.onClick] [span [] [text "Donate"]]
  ]

  event <-  div [Props.classList [Just "donationButton", Just "overlayOpen", Just $ donationLevelClass donationLevel]] [
              div [Props.className "disableOverlay"] [
                div [Props.className "mask", Props.onClick $> CloseDonationPage] []
              , div [Props.className "dialog"] [
                  donationIFrame =<< liftEffect donationIFrameURL
                ]
              ]
            , button [void Props.onClick] [span [] [text "Donate"]] $> CloseDonationPage
            ]
  case event of
    CloseDonationPage -> donationReminder donationLevel
    _                 -> pure event
    
donationUserArea :: Int -> Maybe DonationInfo -> Widget HTML DonationPageEvent
donationUserArea numberOfCards donationInfo = 
  div [Props._id "donationUserArea"] [
    div [Props.className "donationInfo"] $ case donationInfo of
      Just {dateOfLastDonation} -> [
        span [Props.className "dateOfLastDonation"]   [text $ format iso8601DateFormatter dateOfLastDonation]
      , span [Props.className "timeFromLastDonation"] [
          (timeFromLastDonation dateOfLastDonation    # liftEffect) >>= (\days -> case floor $ unwrap days of
            0 -> strong [] [text "Today!"]
            d -> int d <> text " days ago"
          )
        ]
      ]
      Nothing -> [ text "You have never donated 🥹" ]
      <>
      [span [Props.className "nextDonationReminder"] $ case computeNextDonationReminderDate numberOfCards donationInfo of
        Right nextDonationReminder -> [
          span [Props.className "timeUntilNextReminder"] [(timeUntilNextReminder nextDonationReminder # liftEffect) >>= (\days -> let nDays = ceil $ unwrap days in
            if (nDays > 0)
            then int nDays <> text " days"
            else text ""
          )]
        , span [Props.className "dateOfNextReminder"] [text $ format iso8601DateFormatter nextDonationReminder]
        ]
        Left maxNumberOfCards -> [
          span [Props.className "numberOfMaxCards"]  [int  maxNumberOfCards                  <> text " cards"]
        , span [Props.className "numberOfCardsLeft"] [int (maxNumberOfCards - numberOfCards) <> text " cards"]
        ]
      ]
  , donationIFrame =<< liftEffect donationIFrameURL
  ]

  where
    timeFromLastDonation :: DateTime -> Effect Days
    timeFromLastDonation date = do
      now <- liftEffect nowDateTime
      pure $ diff now date

    timeUntilNextReminder :: DateTime -> Effect Days
    timeUntilNextReminder date = do
      now <- liftEffect nowDateTime
      pure $ diff date now