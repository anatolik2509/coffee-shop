{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Menu.Db where

import Database.PostgreSQL.Simple (Query, Connection, execute, query, Only (Only))
import Data.Decimal (Decimal)
import Control.Monad (void)
import Dsl.Menu (Item(..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser, field)
import Dsl.KeyedRecord (KeyedRecord (..))
import Database.PostgreSQL.Simple.FromField (FromField (..), FieldParser)
import Data.ByteString.Char8 (unpack)
import Control.Exception (throw)
import Dsl.Organization (CoffeeShop)
import Control.Monad.Catch (Exception)

createMenuItemQuery :: Query
createMenuItemQuery= "INSERT INTO menu_item(coffee_shop_id, item_name, price) VALUES (?, ?, ?)"

createMenuItem :: Connection -> Integer -> String -> Double -> IO ()
createMenuItem conn coffeeShopId name price = void $ execute conn createMenuItemQuery (coffeeShopId, name, price)


getCoffeeShopMenuQuery :: Query
getCoffeeShopMenuQuery = "SELECT * FROM menu_item WHERE coffee_shop_id = ?;"

getCoffeeShopMenu :: Connection -> Integer -> IO [KeyedRecord Integer Item]
getCoffeeShopMenu conn coffeeShopId = query conn getCoffeeShopMenuQuery (Only coffeeShopId)


data CoffeeShopIdConversionException = CoffeeShopIdConversionException deriving (Show)

instance Exception CoffeeShopIdConversionException


instance FromField (KeyedRecord Integer CoffeeShop) where
  fromField :: FieldParser (KeyedRecord Integer CoffeeShop)
  fromField _ valueBytes = 
    let idMaybe = read . unpack <$> valueBytes :: Maybe Integer in
    return $ maybe (throw CoffeeShopIdConversionException) Key idMaybe

instance FromRow (KeyedRecord Integer Item) where
  fromRow :: RowParser (KeyedRecord Integer Item)
  fromRow = do
    menuItemId <- field :: RowParser Integer
    Record menuItemId <$> (Item <$> field <*> field <*> field)
