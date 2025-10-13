module DataModel.File
  ( base64ToFile
  , fileToBase64
  )
  where

import Control.Alt ((<$>))
import Data.DateTime.Instant (Instant)
import Data.Function (($))
import Data.Maybe (Maybe, fromMaybe)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Web.File.File (File)

-- | Convert a File to its base64 representation
foreign import fileToBase64Impl :: File -> EffectFnAff String

-- | Create a File from a base64 string
foreign import base64ToFileImpl :: String -> String -> String -> Instant -> File

-- | Convert a File to its base64 representation (Promise-based)
fileToBase64 :: File -> Aff String
fileToBase64 file = fromEffectFnAff $ fileToBase64Impl file

-- | Create a File from a base64 string and metadata
base64ToFile :: String -> String -> (Maybe MediaType) -> Instant -> File
base64ToFile base64Content name type_ lastModified = base64ToFileImpl base64Content name (fromMaybe "" $ unwrap <$> type_) lastModified