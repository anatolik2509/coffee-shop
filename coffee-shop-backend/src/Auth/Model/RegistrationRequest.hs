{-# LANGUAGE DeriveGeneric #-}

module Auth.Model.RegistrationRequest where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import qualified Dsl.User as U

data RegistrationForm = RegistrationForm {
    name     :: String,
    lastName :: String,
    phone    :: Maybe String,
    email    :: String,
    password :: String
} deriving (Generic, Show)

instance FromJSON RegistrationForm

registrationFormToUser :: RegistrationForm -> U.User
registrationFormToUser form = U.User {
    U.role = U.UserRole,
    U.name = name form,
    U.lastName = lastName form,
    U.phone = phone form,
    U.email = email form
}
