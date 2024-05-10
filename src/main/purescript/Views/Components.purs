module Views.Components
  ( ClassName(..)
  , Enabled(..)
  , InputType(..)
  , Label(..)
  , Placeholder(..)
  , dynamicWrapper
  , entropyMeter
  , footerComponent
  , proxyInfoComponent
  , verySimpleInputWidget
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, div, footer, input, label, span, text)
import Concur.React.Props as Props
import Control.Plus (empty)
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup ((<>))
import Data.Semiring ((*))
import Data.Show (show)
import Data.Unit (unit)
import Functions.Password (computePasswordEntropy, passwordStrengthClass, standardPasswordStrengthFunction)
import DataModel.Proxy (DataOnLocalStorage(..), ProxyInfo(..))
import Functions.State (_readStaticOfflineCopyTimestamp)

newtype ClassName = ClassName String
derive instance newtypeCharacterSet :: Newtype ClassName _
newtype Label = Label String
newtype Enabled = Enabled Boolean
newtype Placeholder = Placeholder String
newtype InputType = InputType String

verySimpleInputWidget :: InputType -> ClassName -> Label -> Enabled -> Placeholder -> (String -> Maybe ClassName) -> String -> Widget HTML String
verySimpleInputWidget (InputType t) (ClassName className) (Label lbl) (Enabled enabled) (Placeholder placeholder) dynamicClassName value = do
  let c = dynamicClassName value :: Maybe ClassName
  let c' = unwrap <$> c :: Maybe String
  label [Props.classList [Just className, c']] [
    span [Props.className "label"] [text lbl]
  , (Props.unsafeTargetValue) <$> input [
      Props._type t
    , Props.placeholder placeholder
    , Props.value value
    , Props.disabled (not enabled)
    , Props.onChange
    ]
  ]

dynamicWrapper :: forall a. Maybe String -> String -> Widget HTML a -> Widget HTML a
dynamicWrapper maybeClass content elem = div [Props.classList [Just "dynamicWrap", maybeClass], Props.unsafeMkProp "replicatedvalue" content] [ elem ]

entropyMeter :: forall a. String -> Widget HTML a
entropyMeter password = 
  let 
      entropy = computePasswordEntropy password
      strength = show ((entropy) / 256.0 * 100.0)

  in div [Props.classList [Just "entropyWrapper", Just $ passwordStrengthClass (standardPasswordStrengthFunction password)], Props.style {width: strength <> "%"}] []


footerComponent :: forall a. String -> Widget HTML a
footerComponent commit =
  footer [] [
    div [Props.className "footerContent"] [
      div [Props.className "applicationVersion"] [
        span [] [text "application version"]
      , a [Props.href ("https://github.com/clipperz/password-manager/commit/" <> commit), Props.target "_black"] [text commit]
      ]
    ]
  ]

proxyInfoComponent :: ProxyInfo -> Array (Maybe String) -> forall a. Widget HTML a
proxyInfoComponent proxyInfo classes = case proxyInfo of
  Online            ->  empty
  Static            ->  div [Props.classList ([Just "proxyInfo", Just "STATIC"] <> classes)] [
                          span [Props.className "proxyDescription"] [text $ "Static offline copy"]
                        , span [Props.className "proxyDetails"]     [text $ _readStaticOfflineCopyTimestamp unit]
                        ]
  Offline WithData  ->  div [Props.classList ([Just "proxyInfo", Just "OFFLINE WITH_DATA"] <> classes)] [
                          span [Props.className "proxyDescription"] [text $ "No network connection"]
                        , span [Props.className "proxyDetails"]     [text $ ""] -- TODO: read timestamp of last saved data in localstorage (index) [fsolaroli - 25/04/2024]
                        ]
  Offline NoData    ->  div [Props.classList ([Just "proxyInfo", Just "OFFLINE NO_DATA"] <> classes)] [
                          span [Props.className "proxyDescription"] [text $ "No network connection, no local storage data available"]
                        ]