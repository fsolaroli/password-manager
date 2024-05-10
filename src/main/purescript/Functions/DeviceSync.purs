module Functions.DeviceSync where

import Control.Alt ((<#>), (<$>))
import Control.Alternative (pure)
import Control.Bind (bind, (=<<), (>>=))
import Control.Category ((<<<))
import Control.Monad.Except (ExceptT, throwError)
import Control.Semigroupoid ((>>>))
import Data.Argonaut.Parser (jsonParser)
import Data.Codec (decode)
import Data.Either (hush)
import Data.Eq (eq)
import Data.Function ((#), ($))
import Data.HexString (Base(..), HexString, toString)
import Data.Lens (view)
import Data.List (List(..), fold, (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Unit (Unit)
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.AppState (AppState)
import DataModel.IndexVersions.Index (_cardReference_reference, _entries)
import DataModel.UserVersions.User (_indexReference_refence, _userInfoReference_reference, requestUserCardCodec)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.Users (computeRemoteUserCard)
import OperationalWidgets.Sync (SyncOperation(..))
import Views.DeviceSyncView (EnableSync)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (Storage, getItem, removeItem, setItem)

enableSyncKey :: HexString -> String
enableSyncKey c = "enableSync_" <> toString Hex c

getSyncOptionFromLocalStorage :: HexString -> Effect Boolean
getSyncOptionFromLocalStorage c = 
  (getItem (enableSyncKey c) =<< localStorage =<< window) <#> isJust

updateSyncPreference :: HexString -> EnableSync -> Effect Unit
updateSyncPreference c = if _
  then setItem    (enableSyncKey c) "" =<< localStorage =<< window
  else removeItem (enableSyncKey c)    =<< localStorage =<< window

computeSyncOperations :: AppState -> ExceptT AppError Aff (List SyncOperation)
computeSyncOperations {enableSync: false} = pure Nil
computeSyncOperations {c: Just c, p: Just p, s: Just s, masterKey: Just masterKey, srpConf, userInfoReferences: Just userInfoReferences, userInfo: Just userInfo, index: Just index} = do
  storage <- liftEffect $ localStorage =<< window
  user    <- computeRemoteUserCard c p s masterKey srpConf
  let userOperation = (getItem ("user_" <> toString Hex c) storage)
                        <#> (\maybe -> maybe >>= (jsonParser >>> hush) >>= (decode requestUserCardCodec >>> hush))
                        <#> ((eq (Just user)) >>> if _ then Nil else (SaveUser user : Nil)) :: Effect (List SyncOperation)

  liftEffect $ userOperation
            <> checkBlobInStorage storage (view _userInfoReference_reference userInfoReferences)   
            <> checkBlobInStorage storage (view _indexReference_refence      userInfo)
            <> (fold $ view _entries index <#> (checkBlobInStorage storage <<< view _cardReference_reference))

  where
    checkBlobInStorage :: Storage -> HexString -> Effect (List SyncOperation)
    checkBlobInStorage storage ref = getItem ("blob_" <> toString Hex ref) storage # liftEffect <#> (isJust >>> if _ then Nil else (SaveBlob ref : Nil))
      
computeSyncOperations _ = throwError $ InvalidStateError (CorruptedState "State corrupted")

computeDeleteOperations :: AppState -> ExceptT AppError Aff (List SyncOperation)
computeDeleteOperations {c: Just c, userInfoReferences: Just userInfoReferences, userInfo: Just userInfo, index: Just index, enableSync} =
  pure $ if enableSync
        then    DeleteUser c
              : DeleteBlob userInfoReferences.reference
              : DeleteBlob (unwrap (unwrap userInfo).indexReference).reference
              : ((DeleteBlob <<< view _cardReference_reference) <$> view _entries index)
        else    Nil
      
computeDeleteOperations _ = throwError $ InvalidStateError (CorruptedState "State corrupted")