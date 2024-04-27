module Views.DeviceSyncView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, h1, text)

deviceSyncView :: forall a. Widget HTML a -- TODO: implement sync ui [fsolaroli - 27/04/2024]
deviceSyncView = div [] [
  h1 [] [text "Device Sync"]
]