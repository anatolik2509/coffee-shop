{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    ) where

import Config (readConfig, ApplicationConfig(..))
import Web (startScotty)
import System.Environment (getArgs)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Db (startDbModule)
import Database.PostgreSQL.Simple (Connection)
import Web.Scotty (ActionM, ScottyM)
import Data.Pool (Pool)
import Auth (registrationRoute)

parseArgs :: IO FilePath
parseArgs = do
    args <- getArgs
    let wrongArgsCount = error "Wrong arguments count"
    if length args /= 1
        then wrongArgsCount
        else return $ head args

routes :: Pool Connection -> [ScottyM ()]
routes pool = [registrationRoute pool]

initAppWithApplicationConfig :: ReaderT ApplicationConfig IO ()
initAppWithApplicationConfig = startDbModule >>= startScotty . routes 


startApp :: IO ()
startApp = do
    configPath <- parseArgs
    config <- readConfig configPath
    runReaderT initAppWithApplicationConfig config
