
module Functions.Communication.Login where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Alt ((<#>))
import Control.Alternative (guard)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Category ((<<<), (>>>))
import Control.Monad.Except.Trans (ExceptT(..), except, throwError, withExceptT)
import Data.Argonaut.Decode (parseJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt, fromInt)
import Data.Codec.Argonaut (decode, encode)
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), note)
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (Base(..), HexString, fromArrayBuffer, fromBigInt, hexStringCodec, toArrayBuffer, toBigInt, toString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.Communication.ConnectionState (ConnectionState)
import DataModel.Communication.Login (LoginStep1Response, LoginStep2Response, loginStep1ResponseCodec, loginStep2ResponseCodec)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials)
import DataModel.Proxy (DynamicProxy(..), Proxy(..), ProxyResponse(..))
import DataModel.SRPVersions.SRP (SRPConf)
import DataModel.UserVersions.User (MasterKey, RequestUserCard(..), requestUserCardCodec)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.ArrayBuffer (arrayBufferToBigInt)
import Functions.Communication.Backend (isStatusCodeOk, loginRequest)
import Functions.SRP as SRP
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)
    
-- ----------------------------------------------------------------------------

sessionKeyHeaderName :: String
sessionKeyHeaderName = "clipperz-UserSession-ID"

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type PrepareLoginResult = {
  c :: HexString
, p :: HexString
}

prepareLogin :: SRPConf -> Credentials -> ExceptT AppError Aff (PrepareLoginResult)
prepareLogin srpConf { username, password } = do
  c         <- liftAff $ fromArrayBuffer <$> SRP.prepareC srpConf username password
  p         <- liftAff $ fromArrayBuffer <$> SRP.prepareP srpConf username password

  pure {c, p}
  
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type LoginStep1Result = { aa :: BigInt
                        , a  :: BigInt
                        , s  :: HexString
                        , bb :: BigInt
                        }

loginStep1 :: ConnectionState -> HexString -> ExceptT AppError Aff (ProxyResponse LoginStep1Result)

loginStep1 {proxy: proxy@(DynamicProxy (OfflineProxy _ dataOnLocalStorage)), srpConf} c = do
  (Tuple a aa) <- withExceptT (\err -> ProtocolError $ SRPError $ show err) (ExceptT $ SRP.prepareA srpConf)
  RequestUserCard {v, s}         <- ExceptT $ (readUserCard proxy c) # liftEffect
  v'         <- except  $     toBigInt v           #  note (ProtocolError $ SRPError "Cannot covert v from HexString to BigInt") 
  Tuple b bb <- ExceptT $ SRP.prepareB srpConf v' <#> lmap (ProtocolError <<< SRPError <<< show)
  pure $ ProxyResponse (DynamicProxy $ OfflineProxy (Just {b: fromBigInt b, aa: fromBigInt aa, bb: fromBigInt bb}) dataOnLocalStorage) { a, aa, s, bb }

loginStep1 connectionState@{srpConf} c = do
  (Tuple a aa) <- withExceptT (\err -> ProtocolError $ SRPError $ show err) (ExceptT $ SRP.prepareA srpConf)
  let url  = joinWith "/" ["login", "step1", show c] :: String
  let body = json $ encode (CAR.object "loginStep1Request" {c: hexStringCodec, aa: hexStringCodec}) { c, aa: fromBigInt aa }  :: RequestBody
  ProxyResponse newProxy step1Response <- loginRequest connectionState url POST (Just body) RF.json
  responseBody :: LoginStep1Response <- if isStatusCodeOk step1Response.status
                                          then except     $ (decode loginStep1ResponseCodec step1Response.body) # lmap (\err -> ProtocolError $ DecodeError $ show err) 
                                          else throwError $  ProtocolError (ResponseError (unwrap step1Response.status))
  bb :: BigInt <- except $ (toBigInt responseBody.bb) # note (ProtocolError $ SRPError "Error in converting B from String to BigInt")
  if bb == fromInt (0)
    then throwError $ ProtocolError (SRPError "Server returned B == 0")
    else pure $ ProxyResponse newProxy { aa, a, s: responseBody.s, bb }

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type LogintStep2Data = { aa :: BigInt
                       , bb :: BigInt
                       , a  :: BigInt
                       , s  :: HexString
                       }

type LoginStep2Result = { m1 :: ArrayBuffer
                        , kk :: ArrayBuffer
                        , m2 :: HexString
                        , masterKey :: MasterKey
                        }

loginStep2 :: ConnectionState -> HexString -> HexString -> LogintStep2Data -> ExceptT AppError Aff (ProxyResponse LoginStep2Result)

loginStep2 {proxy: proxy@(DynamicProxy (OfflineProxy (Just {b, bb: bb_, aa: aa_}) _)), srpConf} c p { aa, bb, a, s } = do
  x  :: BigInt      <-  ExceptT $ (srpConf.kdf srpConf.hash (toArrayBuffer s) (toArrayBuffer p)) <#> (\ab -> note (ProtocolError $ SRPError "Cannot convert x from ArrayBuffer to BigInt") (arrayBufferToBigInt ab))
  ss :: BigInt      <- (ExceptT $ SRP.prepareSClient srpConf aa bb x a) # withExceptT (\err -> ProtocolError $ SRPError $ show err)
  kk :: ArrayBuffer <-  liftAff $ SRP.prepareK  srpConf ss
  m1 :: ArrayBuffer <-  liftAff $ SRP.prepareM1 srpConf c s aa bb kk
  RequestUserCard {v, c: c_, s: s_, masterKey} <- ExceptT $ (readUserCard proxy c) # liftEffect
  b'     <- except  $     toBigInt b                #  note (ProtocolError  $  SRPError "Cannot covert b from HexString to BigInt") 
  bb'    <- except  $     toBigInt bb_               #  note (ProtocolError  $  SRPError "Cannot covert b from HexString to BigInt") 
  aa'    <- except  $     toBigInt aa_               #  note (ProtocolError  $  SRPError "Cannot covert b from HexString to BigInt") 
  v'     <- except  $     toBigInt v                #  note (ProtocolError  $  SRPError "Cannot covert v from HexString to BigInt") 
  u      <- ExceptT $ SRP.prepareU srpConf aa' bb' <#> lmap (ProtocolError <<< SRPError <<< show)
  secret <- pure    $ SRP.computeSServer srpConf aa' v' b' u
  kk_     <- liftAff $ SRP.prepareK srpConf secret
  _      <- ExceptT $ (SRP.checkM1  srpConf c_ s_ aa' bb' kk_ m1) <#> (guard >>> note (ProtocolError $ SRPError "check m1 failed"))
  m2     <- liftAff $ fromArrayBuffer <$> (SRP.prepareM2 srpConf aa' m1 kk)
  pure $ ProxyResponse proxy { m1, kk, m2, masterKey}

loginStep2 {proxy: (DynamicProxy (OfflineProxy Nothing _))} _ _ _ = throwError $ InvalidStateError (MissingValue "Cannot find backend session state")

loginStep2 connectionState@{srpConf} c p { aa, bb, a, s } = do
  x  :: BigInt      <-  ExceptT $ (srpConf.kdf srpConf.hash (toArrayBuffer s) (toArrayBuffer p)) <#> (\ab -> note (ProtocolError $ SRPError "Cannot convert x from ArrayBuffer to BigInt") (arrayBufferToBigInt ab))
  ss :: BigInt      <- (ExceptT $ SRP.prepareSClient srpConf aa bb x a) # withExceptT (\err -> ProtocolError $ SRPError $ show err)
  kk :: ArrayBuffer <-  liftAff $ SRP.prepareK  srpConf ss
  m1 :: ArrayBuffer <-  liftAff $ SRP.prepareM1 srpConf c s aa bb kk
  let url  = joinWith "/" ["login", "step2", show c]      :: String
  let body = json $ encode (CAR.object "loginStep2Request" {m1: hexStringCodec}) { m1: fromArrayBuffer m1 } :: RequestBody
  ProxyResponse newProxy step2Response <- loginRequest connectionState url POST (Just body) RF.json
  responseBody :: LoginStep2Response   <- if isStatusCodeOk step2Response.status
                                          then except $     (decode loginStep2ResponseCodec step2Response.body) # lmap (\err -> ProtocolError $ DecodeError $ show err)
                                          else throwError $  ProtocolError $ ResponseError (unwrap step2Response.status)
  pure $ ProxyResponse newProxy { m1, kk, m2: responseBody.m2, masterKey: responseBody.masterKey }

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

readUserCard :: Proxy -> HexString -> Effect (Either AppError RequestUserCard)
readUserCard proxy c = do
  case proxy of
    (DynamicProxy (OfflineProxy _ _)) -> window >>= localStorage >>= getItem ("user_" <> (toString Hex c)) <#> note (InvalidStateError $ MissingValue "user not found")
    _                                 -> pure $ Left (InvalidStateError $ MissingValue "user not found")

  <#> (\userCard -> userCard 
                >>= (parseJson                   >>> lmap (ProtocolError <<< DecodeError <<< show)) 
                >>= (decode requestUserCardCodec >>> lmap (ProtocolError <<< DecodeError <<< show)) 
      )
