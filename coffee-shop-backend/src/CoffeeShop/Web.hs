{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoffeeShop.Web where
    
import CoffeeShop.Service (createCoffeeShop, getCoffeeShops)
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool, withResource)
import Web.Scotty (ActionM, get, post, jsonData, status, ScottyM, header, json)
import Auth.Service (getUserByToken)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Exception (throw, Exception)
import Auth.Db (NoSuchUserException(NoSuchUserException))
import Data.Maybe (fromMaybe)
import CoffeeShop.Model.CreateCoffeeShopRequest (requestToCoffeeShop, CreateCoffeeShopRequest (CreateCoffeeShopRequest))
import Control.Monad.Catch (catch, catches, Handler(..))
import Network.HTTP.Types (status403, status500, status200)
import Auth.Web (MissedTokenException(..))
import qualified Data.Text.Lazy as T
import Dsl.KeyedRecord (toTuple, getKey)
import CoffeeShop.Model.CoffeeShopsResponse (coffeeShopsToResponse)


createCoffeeShopRoute :: Pool Connection -> ScottyM ()
createCoffeeShopRoute pool = post "/admin/coffeeShops" $ do
    tokenText <- header "X-Auth-Token"
    let token = maybe (throw MissedTokenException) T.unpack tokenText
    (userId, userRecordMaybe) <- liftIO $ withResource pool $ \conn -> toTuple <$> getUserByToken conn token
    _ <- liftIO $ print userId
    form <- jsonData :: ActionM CreateCoffeeShopRequest
    let coffeeShop = requestToCoffeeShop form userId
    _ <- liftIO $ withResource pool $ \conn -> createCoffeeShop conn coffeeShop
    let result = status status200
    catches result [Handler (\(_ :: NoSuchUserException)  -> status status403),
                    Handler (\(_ :: MissedTokenException) -> status status403)
                   ]


getCoffeeShopsRoute :: Pool Connection -> ScottyM ()
getCoffeeShopsRoute pool = get "/coffeeShops" $ do
    coffeeShops <- liftIO $ withResource pool $ \conn -> getCoffeeShops conn
    json $ coffeeShopsToResponse coffeeShops
