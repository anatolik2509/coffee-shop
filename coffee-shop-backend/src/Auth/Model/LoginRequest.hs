{-# LANGUAGE DeriveGeneric #-}

module Auth.Model.LoginRequest where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data LoginRequest = LoginRequest {
    email :: String,
    password :: String
} deriving (Generic, Show)

instance FromJSON LoginRequest
