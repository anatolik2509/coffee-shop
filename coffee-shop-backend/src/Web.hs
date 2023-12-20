module Web(startScotty, makeRoutes) where

import Web.Scotty
import Config (ApplicationConfig(..))
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool)
import Auth.Web (registrationRoute, signUpRoute, checkTokenRoute)
import CoffeeShop.Web (createCoffeeShopRoute, getCoffeeShopsRoute)

startScotty :: [ScottyM ()] -> ReaderT ApplicationConfig IO ()
startScotty routes = do
    appConfig <- ask
    let appPort = fromMaybe 8080 $ port appConfig
    liftIO $ scotty appPort $ mconcat routes

makeRoutes :: Pool Connection -> [ScottyM ()]
makeRoutes pool = fmap ($ pool) 
                        [registrationRoute, signUpRoute, checkTokenRoute, createCoffeeShopRoute, getCoffeeShopsRoute]
