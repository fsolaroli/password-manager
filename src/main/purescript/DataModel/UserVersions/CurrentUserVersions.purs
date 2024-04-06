module DataModel.UserVersions.CurrentUserVersions where

import Data.Codec.Argonaut (JsonCodec)
import DataModel.UserVersions.User (MasterKeyEncodingVersion(..))
import DataModel.UserVersions.UserCodecs (UserInfo_V3, userInfoV3Codec)

currentMasterKeyEncodingVersion :: MasterKeyEncodingVersion
currentMasterKeyEncodingVersion = MasterKeyEncodingVersion_3

currentUserInfoCodecVersion :: JsonCodec UserInfo_V3
currentUserInfoCodecVersion = userInfoV3Codec
