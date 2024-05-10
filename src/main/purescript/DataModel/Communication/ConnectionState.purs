module DataModel.Communication.ConnectionState where

import Data.HexString (HexString)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import DataModel.Proxy (Proxy)
import DataModel.SRPVersions.SRP (HashFunction, SRPConf)
import Type.Proxy as Proxy

type ConnectionState = {
  proxy    :: Proxy
, hashFunc :: HashFunction
, srpConf  :: SRPConf
, c        :: HexString
, p        :: HexString
}

_proxy :: Lens' ConnectionState Proxy
_proxy = prop (Proxy.Proxy :: _ "proxy")