module DataModel.WidgetState where

import Data.Bounded (class Ord)
import Data.Either (Either)
import Data.Eq (class Eq)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import DataModel.AppState (ProxyInfo)
import DataModel.CardVersions.Card (Card)
import DataModel.Credentials (Credentials)
import DataModel.IndexVersions.Index (CardEntry, Index)
import DataModel.UserVersions.User (UserPreferences, DonationInfo)
import Functions.Donations (DonationLevel)
import IndexFilterView (FilterData)
import Views.CreateCardView (CardFormData)
import Views.OverlayView (OverlayInfo)
import Views.SignupFormView (SignupDataForm)
import Web.File.File (File)

data Page = Loading (Maybe Page) | Login LoginFormData | Signup SignupDataForm | Main MainPageWidgetState | Donation DonationLevel

-- ========================================================================

type PIN = String

data LoginType = CredentialLogin | PinLogin

type LoginFormData = 
  { credentials :: Credentials
  , pin :: PIN
  , loginType :: LoginType
  }

-- ========================================================================

type UserAreaState = {
  showUserArea     :: Boolean
, userAreaOpenPage :: UserAreaPage
, importState      :: ImportState
, userAreaSubmenus :: Map UserAreaSubmenu Boolean
}

data UserAreaPage = Export | Import | Delete | Preferences | ChangePassword | Pin | DeviceSync | Donate | About | None
derive instance eqUserAreaPage :: Eq UserAreaPage

data ImportStep = Upload | Selection | Confirm

type ImportState = {
  step      :: ImportStep
, content   :: Either (Maybe File) String
, selection :: Array (Tuple Boolean Card)
, tag       :: Tuple Boolean String
}

data UserAreaSubmenu = Account | Device | Data
derive instance  eqUserAreaSubmenus :: Eq  UserAreaSubmenu
derive instance ordUserAreaSubmenus :: Ord UserAreaSubmenu

-- ========================================================================

type MainPageWidgetState = {
  index              :: Index
, credentials        :: Credentials
, donationInfo       :: Maybe DonationInfo
, pinExists          :: Boolean
, userAreaState      :: UserAreaState
, cardManagerState   :: CardManagerState
, userPreferences    :: UserPreferences
, donationLevel      :: DonationLevel
}

data WidgetState = WidgetState OverlayInfo Page ProxyInfo

-- -------------------------------------

data CardFormInput = NewCard | NewCardFromFragment Card | ModifyCard Card CardEntry
derive instance eqCardFormInput :: Eq CardFormInput

data CardViewState = NoCard | Card Card CardEntry | CardForm CardFormData CardFormInput
derive instance eqCardViewState :: Eq CardViewState

type CardManagerState = { 
  filterData          :: FilterData
, highlightedEntry    :: Maybe Int
, cardViewState       :: CardViewState
, showShortcutsHelp   :: Boolean
, showDonationOverlay :: Boolean
}