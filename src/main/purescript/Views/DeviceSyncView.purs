module Views.DeviceSyncView where

import Concur.Core (Widget)
import Concur.Core.Patterns (Wire, with)
import Concur.React (HTML)
import Concur.React.DOM (div, form, h1, h2, input, label, text)
import Concur.React.Props as Props
import Control.Alt (($>))
import Control.Alternative (empty)
import Data.CommutativeRing ((*), (+))
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.HeytingAlgebra (not)
import Data.Int (toNumber)
import Data.List (length, null)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import OperationalWidgets.Sync (SyncData)

type EnableSync = Boolean

deviceSyncView :: EnableSync -> Maybe (Wire (Widget HTML) SyncData) -> Widget HTML EnableSync -- TODO: implement sync ui [fsolaroli - 27/04/2024]
deviceSyncView enableSync syncDataWire = div [] [
  h1 [] [text "Device Sync"]
, form [] [
    label [] [
      input [Props._type "checkbox", Props.checked enableSync, Props.onChange $> (not enableSync)]
    ]
  ]
, syncProgressBar enableSync syncDataWire
]

syncProgressBar :: forall a. EnableSync -> Maybe (Wire (Widget HTML) SyncData) -> Widget HTML a
syncProgressBar true (Just wire) = with wire \{completedOperations, pendingOperations} -> do
  if null pendingOperations
  then
    h2 [] [text "Local Storage is synced!"]
  else
    div [Props.className "syncProgressBar"] [
      div [Props.className "completedOperation", Props.style {width: show ((toNumber completedOperations) / (toNumber $ completedOperations + length pendingOperations) * 100.0) <> "%"}] []
    ]
syncProgressBar _ _ = empty