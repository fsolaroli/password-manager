module Views.DeviceSyncView where

import Concur.Core (Widget)
import Concur.Core.Patterns (Wire, with)
import Concur.React (HTML)
import Concur.React.DOM (button, div, form, h1, h2, p, strong, text)
import Concur.React.Props as Props
import Control.Alt (($>))
import Control.Alternative (empty)
import Data.CommutativeRing ((*), (+))
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.Int (toNumber)
import Data.List (length, null)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import OperationalWidgets.Sync (SyncData)

type EnableSync = Boolean

deviceSyncView :: EnableSync -> Maybe (Wire (Widget HTML) SyncData) -> Widget HTML EnableSync
deviceSyncView enableSync syncDataWire = div [Props._id "deviceSync"] [
  form [] [
    h1 [] [ text "Device Sync" ]
  , div [Props.className "description"] [
      p [] [
        text "You may sync all your ", strong [] [text "encrypted"], text " data in the local storage of the current browser you are using. "
      , text "This will allow you to access them even without an internet."
      ]
    ]
  , div [Props.className "content"] [
        case enableSync of
          true  -> do
            syncProgressBar enableSync syncDataWire
            <>
            button [Props.onClick $> false] [text "Remove synched data"]
          false -> do
            button [Props.onClick $> true]  [text "Synch"]
      ]
  ]
]

syncProgressBar :: forall a. EnableSync -> Maybe (Wire (Widget HTML) SyncData) -> Widget HTML a
syncProgressBar true (Just wire) = with wire \{completedOperations, pendingOperations} -> do
  let syncCompleted   = null pendingOperations
      totalOperations = completedOperations + length pendingOperations
      percentageCompleted = if syncCompleted
                            then 100.0
                            else (toNumber completedOperations) / (toNumber totalOperations) * 100.0
    
  ((h2 [] [ if syncCompleted 
            then text "All data synced!"
            else text (show completedOperations <> "/" <> (show $ totalOperations) <> " operations completed")
  ])
  <>
  div [Props.classList [Just "syncProgressBar", if syncCompleted then Just "syncCompleted" else Nothing]] [
    div [Props.className "completedOperation", Props.style {width: show percentageCompleted <> "%"}] []
  ])

syncProgressBar _ _ = empty