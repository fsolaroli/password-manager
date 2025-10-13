module DataModel.CommonCodec where

import Control.Bind ((>>=))
import Control.Category ((<<<), (>>>))
import Data.Bifunctor (lmap, rmap)
import Data.Codec.Argonaut (JsonDecodeError(..), codec', decode, encode)
import Data.Codec.Argonaut as CA
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, fromDateTime, toDateTime)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format, unformat)
import Data.List (List(..), (:))

iso8601DateFormatter :: Formatter
iso8601DateFormatter = YearFull : Placeholder "-" : MonthTwoDigits : Placeholder "-" : DayOfMonthTwoDigits : Nil

dateTimeCodec :: CA.JsonCodec DateTime
dateTimeCodec = codec' (\json -> decode CA.string json >>= (lmap TypeMismatch <<< unformat iso8601DateFormatter)) (format iso8601DateFormatter >>> encode CA.string)

instantCodec :: CA.JsonCodec Instant
instantCodec = codec' (decode dateTimeCodec >>> rmap fromDateTime) (toDateTime >>> encode dateTimeCodec)