{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    ) where

import Config (readConfig, ApplicationConfig(..))
import Web (startScotty, makeRoutes)
import System.Environment (getArgs)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Db (startDbModule)
import Database.PostgreSQL.Simple (Connection)
import Web.Scotty (ActionM, ScottyM)
import Data.Pool (Pool)
import Auth.Web (registrationRoute)

parseArgs :: IO FilePath
parseArgs = do
    args <- getArgs
    let wrongArgsCount = error "Wrong arguments count"
    if length args /= 1
        then wrongArgsCount
        else return $ head args

initAppWithApplicationConfig :: ReaderT ApplicationConfig IO ()
initAppWithApplicationConfig = startDbModule >>= startScotty . makeRoutes


startApp :: IO ()
startApp = do
    configPath <- parseArgs
    config <- readConfig configPath
    runReaderT initAppWithApplicationConfig config
