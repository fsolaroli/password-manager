module Functions.Donations where

import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.List (length)
import Data.Maybe (Maybe(..))
import Data.Ord ((<), (>=))
import DataModel.IndexVersions.Index (Index(..))
import DataModel.UserVersions.User (UserInfo(..))
import Effect (Effect)
import Effect.Now (nowDateTime)

data DonationLevel = DonationOk | DonationInfo | DonationWarning

derive instance eqDonationLevel :: Eq DonationLevel

type NumberOfCards = Int

lowerBoundaryNumberOfCards :: NumberOfCards
lowerBoundaryNumberOfCards = 10

upperBoundaryNumberOfCards :: NumberOfCards
upperBoundaryNumberOfCards = 50

computeDonationLevel :: Index -> UserInfo -> Effect DonationLevel
computeDonationLevel (Index {entries}) (UserInfo {donationInfo}) = do
  let numberOfCards = length entries
  now <- nowDateTime
  pure $ case donationInfo of
    Just {nextDonationReminder} | now < nextDonationReminder -> DonationOk
    _ ->   if numberOfCards <  lowerBoundaryNumberOfCards  then DonationOk
      else if numberOfCards >= upperBoundaryNumberOfCards  then DonationWarning
      else                                                      DonationInfo

donationLevelClass :: DonationLevel -> String
donationLevelClass donationLevel = case donationLevel of
  DonationOk      -> "DonationOk"
  DonationInfo    -> "DonationInfo"
  DonationWarning -> "DonationWarning"