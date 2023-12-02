module Dsl.User(
    Role(..), User(..)
) where

data Role = UserRole | ManagerRole | OrganizationRole

data User = User {
    role     :: Role,
    name     :: String,
    lastName :: String,
    phone    :: String,
    email    :: String
}
