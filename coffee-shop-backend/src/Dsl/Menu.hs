module Dsl.Menu(Item(..)) where

import Dsl.KeyedRecord
import Dsl.Organization(CoffeeShop)

data Item = Item {
    coffeeShop :: KeyedRecord Integer CoffeeShop,
    itemName :: String,
    price :: Double
}
