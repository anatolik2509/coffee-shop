module Dsl.Menu(Item(..)) where

import Data.Decimal
import Dsl.KeyedRecord
import Dsl.Organization(CoffeeShop)

data Item = Item {
    coffeeShop :: KeyedRecord Integer CoffeeShop,
    itemName :: String,
    price :: Decimal,
    imageUrl :: Maybe String
}
