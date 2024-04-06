module Functions.EnvironmentalVariables
  ( currentCommit
  , shareURL
  , redeemURL
  , appURL
  , donationIFrameURL
  )
  where

import Data.Unit (Unit, unit)
import Effect (Effect)

foreign import _currentCommit :: Unit -> Effect String
currentCommit :: Effect String
currentCommit = _currentCommit unit

foreign import _shareURL :: Unit -> Effect String
shareURL :: Effect String
shareURL = _shareURL unit


foreign import _redeemURL :: Unit -> Effect String
redeemURL :: Effect String
redeemURL = _redeemURL unit


foreign import _appURL :: Unit -> Effect String
appURL :: Effect String
appURL = _appURL unit


foreign import _donationIFrameURL :: Unit -> Effect String
donationIFrameURL :: Effect String
donationIFrameURL = _donationIFrameURL unit

