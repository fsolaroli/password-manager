module Functions.Communication.Signup where

import Affjax.Web as AXW
import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), mapExceptT)
import Control.Monad.State (StateT, mapStateT, modify_)
import Control.Semigroupoid ((>>>))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, hex, fromArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.AppState (AppState)
import Effect.Aff (Aff)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.SRP as SRP
import Functions.State (makeStateT)

type UserCard = {
    c :: HexString
  , v :: HexString
  , s :: HexString
  , srpVersion :: String
  , masterKeyEncodingVersion :: String
  , masterKeyContent :: HexString
}
type RegisterUserRequest = {
    user :: UserCard
  , indexCardReference :: HexString
  , indexCardContent   :: HexString
  , cards :: Array (Tuple HexString HexString)
}

registerUser :: RegisterUserRequest -> StateT AppState (ExceptT ProtocolError Aff) HexString
registerUser request = do
  let url = joinWith "/" ["users", show request.user.c]
  let body = (json $ encodeJson request) :: RequestBody
  --- --------------------------- 
  sessionKey :: HexString   <- makeStateT $ ExceptT $ (fromArrayBuffer >>> Right) <$> SRP.randomArrayBuffer 32 --- TODO: maybe to manage with session middleware
  modify_ (\currentState -> currentState { sessionKey = Just sessionKey }) --- TODO: maybe to manage with session middleware
  --- --------------------------- 
  mapStateT (\except -> mapExceptT (\aff -> mapResponse <$> aff) except) (manageGenericRequest url PUT (Just body) RF.string)
  
  where mapResponse :: Either ProtocolError (Tuple (AXW.Response String) AppState)
                    -> Either ProtocolError (Tuple HexString AppState)
        mapResponse (Right (Tuple response state)) = if isStatusCodeOk response.status
                                                     then Right $ Tuple (hex response.body) state
                                                     else Left  (ResponseError (unwrap response.status))
        mapResponse (Left error) = Left error
