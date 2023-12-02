module Dsl.Order(OrderPosition(..), Order(..), OrderState(..)) where
import Dsl.KeyedRecord (KeyedRecord)
import Dsl.Menu (Item)
import Dsl.User (User)
import Data.Decimal (Decimal)
import Dsl.Organization (CoffeeShop)

data OrderState = Creating | Cooking | Ready | Done

data OrderPosition = OrderPosition {
    item :: KeyedRecord Integer Item,
    quantity :: Int,
    order :: KeyedRecord Integer OrderPosition
}

data Order = Order {
    customer :: KeyedRecord Integer User,
    coffeeShop :: KeyedRecord Integer CoffeeShop,
    orderState :: OrderState,
    sumPrice :: Decimal
}
