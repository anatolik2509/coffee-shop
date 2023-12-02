{-# LANGUAGE OverloadedStrings #-}

module Config(ApplicationConfig(..), readConfig) where
import Data.Configurator as C (load, require, lookup, Worth (Required))



data ApplicationConfig = ApplicationConfig {
    port :: Maybe Int,
    databaseHost :: String,
    databasePort :: Int,
    databaseName :: String,
    databaseUser :: String,
    databasePassword :: String,
    migrationsPath :: String
}

readConfig :: FilePath -> IO ApplicationConfig
readConfig path = do
    config <- load [Required path]
    cPort <- C.lookup config "PORT"
    cDatabaseHost <- require config "DB.HOST"
    cDatabasePort <- require config "DB.PORT"
    cDatabaseName <- require config "DB.NAME"
    cDatabaseUser <- require config "DB.USER"
    cDatabasePassword <- require config "DB.PASSWORD"
    cMigrationsPath <- require config "MIGRATIONS.PATH"
    return $ ApplicationConfig {
        port = cPort,
        databaseHost = cDatabaseHost,
        databasePort = cDatabasePort,
        databaseName = cDatabaseName,
        databaseUser = cDatabaseUser,
        databasePassword = cDatabasePassword,
        migrationsPath = cMigrationsPath
    } 
