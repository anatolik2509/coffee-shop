{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoffeeShop.Model.CoffeeShopsResponse where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Dsl.KeyedRecord (KeyedRecord, getKey, getValue)
import qualified Dsl.Organization as O (CoffeeShop (..))

data CoffeeShopsResponse = CoffeeShopsResponse {
    coffeeShops :: [CoffeeShopElement]
} deriving (Show, Generic, ToJSON)

data CoffeeShopElement = CoffeeShopElement {
    id      :: Integer,
    name    :: String,
    address :: String
} deriving (Show, Generic, ToJSON)

coffeeShopsToResponse :: [KeyedRecord Integer O.CoffeeShop] -> CoffeeShopsResponse
coffeeShopsToResponse coffeeShopRecords = 
    let coffeeShopToElement coffeeShopRecord = CoffeeShopElement {
        id = getKey coffeeShopRecord,
        name = maybe "" O.name (getValue coffeeShopRecord),
        address = maybe "" O.address (getValue coffeeShopRecord)
    } in CoffeeShopsResponse $ fmap coffeeShopToElement coffeeShopRecords