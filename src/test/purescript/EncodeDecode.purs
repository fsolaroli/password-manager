module Test.EncodeDecode where

import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Crypto.Subtle.Constants.AES as AES
import Crypto.Subtle.Encrypt as Encrypt
import Crypto.Subtle.Key.Generate as Key.Generate
import Crypto.Subtle.Key.Types as Key.Types
import Data.ArrayBuffer.Typed as Data.ArrayBuffer.Typed
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, Uint8)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Ring ((*))
import Data.Show (show)
import Data.TextDecoder as Decoder
import Data.TextEncoder as Encoder
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Effect.Exception (Error, error)
import Effect.Class (liftEffect)
import Test.Spec (describe, it, SpecT)
import Test.Spec.Assertions (shouldEqual)
import TestUtilities (makeTestableOnBrowser, failOnBrowser)
import Utilities (emptyByteArrayBuffer)

encodeDecodeSpec :: SpecT Aff Unit Identity Unit
encodeDecodeSpec = 
  describe "EncodeDecode" do
    let encryptDecrypt = "encrypt then decrypt"
    it encryptDecrypt do
      let text = "The quick brown fox jumps over the lazy dog" :: String
      let encodedText = Encoder.encode Encoder.Utf8 text :: ArrayView Uint8   -- Uint8Array
      let textBuffer = Data.ArrayBuffer.Typed.buffer encodedText :: ArrayBuffer
      let blockSize' = 16 :: Int
      let blockSizeInBits' = blockSize' * 8 :: Int
      let emptyCounter = emptyByteArrayBuffer blockSize' :: ArrayBuffer
      let algorithm = (Encrypt.aesCTR emptyCounter blockSizeInBits') :: Encrypt.EncryptAlgorithm

      key              :: Key.Types.CryptoKey    <- Key.Generate.generateKey (Key.Generate.aes AES.aesCTR AES.l256) false [Key.Types.encrypt, Key.Types.decrypt]

      encryptedData     :: ArrayBuffer     <- Encrypt.encrypt algorithm key textBuffer

      decryptedData :: Maybe ArrayBuffer <- Encrypt.decrypt algorithm key encryptedData
      decryptedDataView :: Either Error (ArrayView Uint8) <- case decryptedData of
          Nothing -> pure $ Left (error "Something went wrong")
          Just d  -> liftEffect $ Right <$> (Data.ArrayBuffer.Typed.whole d)

      let decryptedDataText = decryptedDataView >>= (Decoder.decode Decoder.Utf8)  :: (Either Error String)
      case decryptedDataText of 
        Left err -> failOnBrowser encryptDecrypt (show err)
        Right decrypted -> makeTestableOnBrowser encryptDecrypt decrypted shouldEqual text

      