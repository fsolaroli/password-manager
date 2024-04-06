module Functions.Donations where

import Control.Alternative (pure)
import Control.Bind (bind)
import Data.DateTime (DateTime, adjust)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Function (($))
import Data.HeytingAlgebra ((||))
import Data.List (length)
import Data.Maybe (Maybe(..))
import Data.Ord ((<), (>=))
import Data.Time.Duration (Days(..), negateDuration)
import DataModel.IndexVersions.Index (Index(..))
import DataModel.UserVersions.User (UserInfo(..), DonationInfo)
import Effect (Effect)
import Effect.Now (nowDateTime)

data DonationLevel = DonationOk | DonationInfo | DonationWarning

derive instance eqDonationLevel :: Eq DonationLevel

type NumberOfCards = Int

lowerBoundaryNumberOfCards :: NumberOfCards
lowerBoundaryNumberOfCards = 10

upperBoundaryNumberOfCards :: NumberOfCards
upperBoundaryNumberOfCards = 80

infoDaysDiff :: Days
infoDaysDiff =  negateDuration $ Days 15.0

computeDonationLevel :: Index -> UserInfo -> Effect DonationLevel
computeDonationLevel (Index {entries}) (UserInfo {donationInfo}) = do
  let numberOfCards = length entries
  now <- nowDateTime
  pure $ case donationInfo of
    Nothing -> 
           if numberOfCards <  lowerBoundaryNumberOfCards then DonationOk
      else if numberOfCards >= upperBoundaryNumberOfCards then DonationWarning
      else                                                     DonationInfo
    Just {nextDonationReminder} ->
           if Just now <  adjust infoDaysDiff nextDonationReminder || numberOfCards <  lowerBoundaryNumberOfCards then DonationOk
      else if      now >=                     nextDonationReminder                                                then DonationWarning
      else                                                                                                             DonationInfo

computeNextDonationReminderDate :: Int -> Maybe DonationInfo -> Either Int DateTime
computeNextDonationReminderDate numberOfCards donationInfo = do
  case donationInfo of
    Just {nextDonationReminder} -> 
      if numberOfCards < lowerBoundaryNumberOfCards
      then  Left  lowerBoundaryNumberOfCards
      else  Right nextDonationReminder
    Nothing -> 
            Left  upperBoundaryNumberOfCards

donationLevelClass :: DonationLevel -> String
donationLevelClass donationLevel = case donationLevel of
  DonationOk      -> "DonationOk"
  DonationInfo    -> "DonationInfo"
  DonationWarning -> "DonationWarning"