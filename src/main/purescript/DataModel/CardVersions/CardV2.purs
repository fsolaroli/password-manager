module DataModel.CardVersions.CardV2 where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime.Instant (Instant)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.MediaType (MediaType(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (wrapIso)
import Data.Set (Set)
import DataModel.CardVersions.Card (class CardVersions, Card(..), CardAttachment(..), CardField(..), CardValues(..))
import DataModel.CommonCodec (instantCodec)

newtype Card_V2 = Card_V2 
  { content :: CardValues_V2
  , secrets :: Array String
  , archived :: Boolean
  , timestamp :: Number
  }
cardV2Codec :: CA.JsonCodec Card_V2
cardV2Codec = wrapIso Card_V2 $
  CAR.object "cardV2"
    { content   : cardValuesV2Codec
    , secrets   : CA.array CA.string
    , archived  : CA.boolean
    , timestamp : CA.number
    }

derive instance newtypeCard_V2 :: Newtype Card_V2 _

instance card_v2 :: CardVersions Card_V2 where
  toCard (Card_V2 card) = Card card { content = CardValues card.content { fields = CardField <$> card.content.fields, attachments = CardAttachment <$> card.content.attachments } }
  fromCard (Card card@{content: CardValues content@{fields}}) = Card_V2 card {content = content {fields = unwrap <$> fields, attachments = unwrap <$> content.attachments}}

-- ---------------------------------------------------------

type CardValues_V2 = 
  { title       :: String
  , tags        :: Set String
  , fields       :: Array CardField_V2
  , notes       :: String
  , attachments :: Array CardAttachment_V2
  }
cardValuesV2Codec :: CA.JsonCodec CardValues_V2
cardValuesV2Codec = 
  CAR.object "cardValuesV2"
    { title       : CA.string
    , tags        : CAC.set CA.string
    , fields       : CA.array cardFieldV2Codec
    , notes       : CA.string
    , attachments : CA.array cardFileV2Codec
    }

-- ---------------------------------------------------------

type CardAttachment_V2 =
  { base64Encoding :: String
  , name :: String
  , type_ :: Maybe MediaType
  , lastModified :: Instant
  }

cardFileV2Codec :: CA.JsonCodec CardAttachment_V2
cardFileV2Codec = 
  CAR.object "CardAttachment_V2"
    { base64Encoding : CA.string
    , name           : CA.string
    , type_          : CAR.optional $ wrapIso MediaType CA.string
    , lastModified   : instantCodec
    }

-- ---------------------------------------------------------

type CardField_V2 =
  { name     :: String
  , value    :: String
  , locked   :: Boolean
  , settings :: Maybe PasswordGeneratorSettings_V2
  }
cardFieldV2Codec :: CA.JsonCodec CardField_V2
cardFieldV2Codec =
  CAR.object "cardFieldV2"
    { name     : CA.string
    , value    : CA.string
    , locked   : CA.boolean
    , settings : CAR.optional passwordGeneratorSettingsV2Codec
    }

-- ---------------------------------------------------------

type PasswordGeneratorSettings_V2 = {
    length     :: Int,
    characters :: String
}
passwordGeneratorSettingsV2Codec :: CA.JsonCodec PasswordGeneratorSettings_V2
passwordGeneratorSettingsV2Codec = 
  CAR.object "PasswordGeneratorSettings_V2"
    { length     : CA.int
    , characters : CA.string
    }
