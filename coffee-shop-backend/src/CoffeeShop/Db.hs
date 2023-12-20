{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CoffeeShop.Db where

import Database.PostgreSQL.Simple (execute, FromRow, Connection, Query, query)
import Control.Monad (void)
import Dsl.Organization (CoffeeShop (..))
import Dsl.KeyedRecord (KeyedRecord (..))
import Database.PostgreSQL.Simple.FromRow (RowParser, FromRow (fromRow), field)
import Database.PostgreSQL.Simple.FromField (FromField (..), FieldParser)
import Dsl.User (User)
import Auth.Db (NoSuchUserException (..))
import Data.ByteString.Char8 (unpack)
import Control.Exception (throw)


addCoffeeShopQuery :: Query
addCoffeeShopQuery = "INSERT INTO coffee_shop (name, address, owner_id) VALUES (?, ?, ?);"

addCoffeeShop :: Connection -> String -> String -> Integer -> IO ()
addCoffeeShop conn name address ownerId = void $ execute conn addCoffeeShopQuery (name, address, ownerId)

getCoffeeShopsQuery :: Query
getCoffeeShopsQuery = "SELECT * FROM coffee_shop;"

getCoffeeShops :: Connection -> IO [KeyedRecord Integer CoffeeShop]
getCoffeeShops conn = query conn getCoffeeShopsQuery ()

instance FromField (KeyedRecord Integer User) where
  fromField :: FieldParser (KeyedRecord Integer User)
  fromField _ valueBytes = 
    let idMaybe = read . unpack <$> valueBytes :: Maybe Integer in
    return $ maybe (throw NoSuchUserException) Key idMaybe

instance FromRow (KeyedRecord Integer CoffeeShop) where
  fromRow :: RowParser (KeyedRecord Integer CoffeeShop)
  fromRow = do
    coffeeShopId <- field :: RowParser Integer
    Record coffeeShopId <$> (CoffeeShop <$> field <*> field <*> field <*> field)
