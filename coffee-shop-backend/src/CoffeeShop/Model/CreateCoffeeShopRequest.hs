{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module CoffeeShop.Model.CreateCoffeeShopRequest where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import qualified Dsl.Organization as O (CoffeeShop (..))
import Dsl.KeyedRecord (KeyedRecord(..))

data CreateCoffeeShopRequest = CreateCoffeeShopRequest {
    name    :: String,
    address :: String
} deriving (Generic, FromJSON)

requestToCoffeeShop :: CreateCoffeeShopRequest -> Integer -> O.CoffeeShop
requestToCoffeeShop request userId = O.CoffeeShop {
    O.name = name request,
    O.address = address request,
    O.logoUrl = Nothing,
    O.owner = Key userId
}
