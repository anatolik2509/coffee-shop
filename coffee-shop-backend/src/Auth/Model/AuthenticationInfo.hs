module Auth.Model.AuthenticationInfo where

import Dsl.User (User)

data AuthenticationInfo = AuthenticationInfo {
    user :: User
}
