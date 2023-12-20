module Dsl.Organization (
    ManagerInfo(..), OrganizationInfo(..), CoffeeShop(..)
) where

import Dsl.KeyedRecord (KeyedRecord)
import Dsl.User (User)


data ManagerInfo = ManagerInfo {
    managerUser :: KeyedRecord Integer User,
    coffeeShop  :: KeyedRecord Integer CoffeeShop
}

data OrganizationInfo = OrganizationInfo {
    orgUser :: KeyedRecord Integer User,
    orgName :: String
}

data CoffeeShop = CoffeeShop {
    name :: String,
    address :: String,
    owner :: KeyedRecord Integer User,
    logoUrl :: Maybe String
}
