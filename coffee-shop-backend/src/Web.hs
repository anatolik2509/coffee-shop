module Web(startScotty) where

import Web.Scotty
import Config (ApplicationConfig(..))
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (fromMaybe)

startScotty :: [ScottyM ()] -> ReaderT ApplicationConfig IO ()
startScotty routes = do
    appConfig <- ask
    let appPort = fromMaybe 8080 $ port appConfig
    liftIO $ scotty appPort $ mconcat routes
