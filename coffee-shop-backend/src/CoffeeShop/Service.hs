module CoffeeShop.Service where
import qualified Dsl.Organization as O (CoffeeShop (name, address, owner))
import Database.PostgreSQL.Simple (Connection)
import qualified CoffeeShop.Db as Db (addCoffeeShop, getCoffeeShops)
import Dsl.KeyedRecord (getKey, KeyedRecord)

createCoffeeShop :: Connection -> O.CoffeeShop -> IO ()
createCoffeeShop conn coffeeShopDto = Db.addCoffeeShop conn (O.name coffeeShopDto) (O.address coffeeShopDto) (getKey $ O.owner coffeeShopDto)

getCoffeeShops :: Connection -> IO [KeyedRecord Integer O.CoffeeShop]
getCoffeeShops = Db.getCoffeeShops
