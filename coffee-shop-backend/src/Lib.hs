{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    ) where

import Config (readConfig, ApplicationConfig(..))
import Web (startScotty)
import System.Environment (getArgs)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Db (startDbModule)

parseArgs :: IO FilePath
parseArgs = do
    args <- getArgs
    let wrongArgsCount = error "Wrong arguments count"
    if length args /= 1
        then wrongArgsCount
        else return $ head args

initAppWithApplicationConfig :: ReaderT ApplicationConfig IO ()
initAppWithApplicationConfig = startDbModule >> startScotty []


startApp :: IO ()
startApp = do
    configPath <- parseArgs
    config <- readConfig configPath
    runReaderT initAppWithApplicationConfig config
