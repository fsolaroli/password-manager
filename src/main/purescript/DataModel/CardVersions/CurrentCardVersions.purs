module DataModel.CardVersions.CurrentCardVersions where

import Data.Codec.Argonaut (JsonCodec)
import DataModel.CardVersions.Card (CardVersion(..))
import DataModel.CardVersions.CardV2 (Card_V2, cardV2Codec)

currentCardVersion :: CardVersion
currentCardVersion = CardVersion_2

currentCardCodecVersion :: JsonCodec Card_V2
currentCardCodecVersion = cardV2Codec