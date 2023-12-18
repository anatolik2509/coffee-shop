{-# LANGUAGE DeriveGeneric #-}
module Auth.Model.LoginResponse where
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

data LoginResponse = LoginResponse {
    token :: String
} deriving (Show, Generic)

instance ToJSON LoginResponse
