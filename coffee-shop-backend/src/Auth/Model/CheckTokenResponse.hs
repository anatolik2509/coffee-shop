{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Auth.Model.CheckTokenResponse where

import Dsl.KeyedRecord (KeyedRecord (..))
import qualified Dsl.User as U (User (..))
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data CheckTokenResponse = CheckTokenResponse {
    userId :: Integer,
    email :: String,
    role :: String
} deriving (Generic, ToJSON)

userToCheckTokenResponse :: KeyedRecord Integer U.User -> CheckTokenResponse
userToCheckTokenResponse (Key k) = CheckTokenResponse k "" ""
userToCheckTokenResponse (Record k v) = CheckTokenResponse k (U.email v) (show $ U.role v)
