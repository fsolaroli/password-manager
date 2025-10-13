module DataModel.CardVersions.Card where

import Control.Alt ((<#>), (<$>))
import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Variant as CAV
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Eq (class Eq, eq, (==))
import Data.Function (($))
import Data.HeytingAlgebra ((&&))
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype)
import Data.Profunctor (dimap)
import Data.Semigroup ((<>))
import Data.Set (Set, empty, fromFoldable)
import Data.Show (class Show, show)
import Data.Unit (unit)
import Data.Variant as V
import DataModel.Password (PasswordGeneratorSettings)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Type.Proxy (Proxy(..))

data CardVersion = CardVersion_1 | CardVersion_2
cardVersionCodec :: CA.JsonCodec CardVersion
cardVersionCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { cardVersion_1: Left unit
    , cardVersion_2: Left unit
    }
  where
    toVariant = case _ of
      CardVersion_1 -> V.inj (Proxy :: _ "cardVersion_1") unit
      CardVersion_2 -> V.inj (Proxy :: _ "cardVersion_2") unit
    fromVariant = V.match
      { cardVersion_1: \_ -> CardVersion_1
      , cardVersion_2: \_ -> CardVersion_2
      }

instance showCardVersion :: Show CardVersion where
 show CardVersion_1 = "CardVersion_1"
 show CardVersion_2 = "CardVersion_2"

-- --------------------------------------------

newtype CardField =
  CardField
    { name   :: String
    , value  :: String
    , locked :: Boolean
    , settings :: Maybe PasswordGeneratorSettings
    }

derive instance newtypeCardField :: Newtype CardField _

instance eqCardField :: Eq CardField where
  eq (CardField r1) (CardField r2) = eq r1 r2

instance showCardField :: Show CardField where
  show (CardField { name, value, locked }) = "[" <> show locked <> "] " <> name <> ": " <> value

instance arbitratryCardField :: Arbitrary CardField where
  arbitrary = CardField <$> arbitrary

-- --------------------------------------------

newtype CardAttachment = 
  CardAttachment
    { base64Encoding :: String
    , name :: String
    , type_ :: Maybe MediaType
    , lastModified :: Instant
    }

derive instance newtypeCardAttachment :: Newtype CardAttachment _

instance eqCardAttachment :: Eq CardAttachment where
  eq (CardAttachment r1) (CardAttachment r2) = 
    r1.base64Encoding == r2.base64Encoding && 
    r1.name == r2.name && 
    r1.type_ == r2.type_ && 
    r1.lastModified == r2.lastModified

instance showCardAttachment :: Show CardAttachment where
  show (CardAttachment record) = show record

-- -------------------------------------------- 

newtype CardValues = 
  CardValues
    { title       :: String
    , tags        :: Set String
    , fields       :: Array CardField
    , notes       :: String
    , attachments :: Array CardAttachment
    }

derive instance newtypeCardValues :: Newtype CardValues _

instance eqCardValues :: Eq CardValues where
  eq (CardValues r1) (CardValues r2) = 
    r1.title == r2.title && 
    r1.tags == r2.tags && 
    r1.fields == r2.fields && 
    r1.notes == r2.notes &&
    r1.attachments == r2.attachments

instance showCardValues :: Show CardValues where
  show (CardValues record) = show record

instance arbitraryCardValues :: Arbitrary CardValues where
  arbitrary = CardValues <$> do
    title  <- arbitrary
    tags   <- arbitrary <#> (\(array :: Array String) -> fromFoldable array)
    fields <- arbitrary
    notes  <- arbitrary
    pure $ {title, tags, fields, notes, attachments: []}

-- --------------------------------------------

newtype Card = 
  Card 
    { content :: CardValues
    , secrets :: Array String
    , archived :: Boolean
    , timestamp :: Number
    }

derive instance newtypeCard :: Newtype Card _

instance eqCard :: Eq Card where
  eq (Card r1) (Card r2) = eq { content: r1.content, archived: r1.archived } { content: r2.content, archived: r2.archived }

instance showCard :: Show Card where
  show (Card record) = show record

class CardVersions a where
  toCard   :: a    -> Card
  fromCard :: Card -> a

instance arbitraryCard :: Arbitrary Card where
  arbitrary = Card <$> arbitrary


-- --------------------------------------------

emptyCardField :: CardField
emptyCardField = CardField { name: "", value: "", locked: false, settings: Nothing }

emptyCard :: Card
emptyCard = Card { timestamp: 0.0
                 , archived: false
                 , secrets: []
                 , content: CardValues { title: ""
                                     , tags: empty
                                     , fields: [ CardField { name: "username", value: "", locked: false, settings: Nothing }
                                             , CardField { name: "password", value: "", locked: true,  settings: Nothing }
                                             ]
                                     , notes: ""
                                     , attachments: []
                                     }
                 }

defaultCards :: List Card
defaultCards = Nil

data FieldType = Email | Url | Passphrase | None


-- ==================================================================

_content :: Lens' Card CardValues
_content = _Newtype <<< prop (Proxy :: _ "content")

_title :: Lens' Card String
_title = _content <<< _Newtype <<< prop (Proxy :: _ "title")

_archived :: Lens' Card Boolean
_archived = _Newtype <<< prop (Proxy :: _ "archived")

_tags :: Lens' Card (Set String)
_tags = _content <<< _Newtype <<< prop (Proxy :: _ "tags")

_fields :: Lens' Card (Array CardField)
_fields = _content <<< _Newtype <<< prop (Proxy :: _ "fields")

_notes :: Lens' Card String
_notes = _content <<< _Newtype <<< prop (Proxy :: _ "notes")

_attachments :: Lens' Card (Array CardAttachment)
_attachments = _content <<< _Newtype <<< prop (Proxy :: _ "attachments")