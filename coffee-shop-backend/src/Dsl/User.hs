{-# LANGUAGE DeriveGeneric #-}

module Dsl.User(
    Role(..), User(..), Credentials(..), CredentialType(..)
) where
import Dsl.KeyedRecord (KeyedRecord)
import Data.Time
import GHC.Generics (Generic)

data Role = UserRole | ManagerRole | OrganizationRole

data User = User {
    role     :: Role,
    name     :: String,
    lastName :: String,
    phone    :: Maybe String,
    email    :: String
} deriving (Generic)

data CredentialType = Password | AccessToken

data Credentials = Credentials {
    user :: KeyedRecord Integer User,
    cred_type :: CredentialType,
    value :: String,
    expireDate :: Maybe UTCTime
}
