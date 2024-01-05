module Functions.Communication.Signup where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Control.Semigroupoid ((>>>))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Function (flip, ($))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import DataModel.AppState (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials)
import DataModel.SRP (SRPConf, HashFunction)
import DataModel.StatelessAppState (Proxy(..), ProxyResponse(..))
import DataModel.User (RequestUserCard(..))
import Effect.Aff (Aff)
import Functions.Communication.Login (PrepareLoginResult)
import Functions.Communication.StatelessBackend (isStatusCodeOk, manageGenericRequest)
import Functions.Signup (prepareSignupParameters)

type SessionKey = HexString

type SignupResult = PrepareLoginResult

signupUser :: Proxy -> HashFunction -> SRPConf -> Credentials -> ExceptT AppError Aff (ProxyResponse SignupResult)
signupUser       (StaticProxy _)     _        _       _           = throwError $ InvalidOperationError "Cannot register new user in static usage"
signupUser proxy@(OnlineProxy _ _ _) hashFunc srpConf credentials = do
  request@{user: RequestUserCard u, p} <- flip withExceptT (ExceptT (prepareSignupParameters srpConf credentials)) (show >>> SRPError >>> ProtocolError)
  let path  = joinWith "/" ["users", show u.c]
  let body = (json $ encodeJson request) :: RequestBody
  --- ---------------------------
  ProxyResponse newProxy response <- manageGenericRequest {proxy, hashFunc} path POST (Just body) RF.string
  if isStatusCodeOk response.status
    then pure $ ProxyResponse newProxy {c: u.c, p: p}
    else throwError $ ProtocolError (ResponseError (unwrap response.status))

