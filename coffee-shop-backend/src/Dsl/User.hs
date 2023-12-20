{-# LANGUAGE DeriveGeneric #-}

module Dsl.User(
    Role(..), User(..), Credentials(..), CredentialType(..)
) where
import Dsl.KeyedRecord (KeyedRecord)
import Data.Time
import GHC.Generics (Generic)

data Role = UserRole | ManagerRole | OrganizationRole deriving (Show, Read)

data User = User {
    role     :: Role,
    name     :: String,
    lastName :: String,
    email    :: String,
    phone    :: Maybe String
} deriving (Generic)

data CredentialType = Password | AccessToken

data Credentials = Credentials {
    user :: KeyedRecord Integer User,
    credType :: CredentialType,
    credValue :: String,
    expireDate :: Maybe UTCTime
}
