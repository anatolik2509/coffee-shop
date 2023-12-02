module Db(startDbModule) where

import Database.PostgreSQL.Simple
import Config (ApplicationConfig (..))
import Data.Word (Word16)
import Data.Pool (PoolConfig, defaultPoolConfig, Pool, newPool, withResource)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory), defaultOptions, runMigrations)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Database.PostgreSQL.Simple.Migration.V1Compat (MigrationResult)
import Control.Monad.IO.Class (MonadIO(liftIO))

createConnectionInfo :: ApplicationConfig -> ConnectInfo
createConnectionInfo appConfig = defaultConnectInfo {
    connectHost = databaseHost appConfig,
    connectPort = fromIntegral (databasePort appConfig) :: Word16,
    connectDatabase = databaseName appConfig,
    connectUser = databaseUser appConfig,
    connectPassword = databasePassword appConfig
}

timeBeforeDestroyConnectionSec :: Double
timeBeforeDestroyConnectionSec = 10.0

maxConnects :: Int
maxConnects = 10

createConnectionPoolConfig :: ConnectInfo -> PoolConfig Connection
createConnectionPoolConfig connectInfo = defaultPoolConfig (connect connectInfo) close timeBeforeDestroyConnectionSec maxConnects

createMigrationCommand :: ApplicationConfig -> MigrationCommand
createMigrationCommand appConfig = MigrationDirectory $ migrationsPath appConfig

executeMigration :: Pool Connection -> MigrationCommand -> IO (MigrationResult String)
executeMigration pool command = withResource pool runCommand
                                    where runCommand conn = runMigrations conn defaultOptions [command]

startDbModule :: ReaderT ApplicationConfig IO (Pool Connection)
startDbModule = do
    appConfig <- ask
    let migrationCommand = createMigrationCommand appConfig
    let connectInfo = createConnectionInfo appConfig
    let poolConfig = createConnectionPoolConfig connectInfo
    pool <- liftIO $ newPool poolConfig
    let _ = executeMigration pool migrationCommand
    return pool
