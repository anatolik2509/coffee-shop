{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Auth where

import Network.Wai.Middleware.HttpAuth
import Database.PostgreSQL.Simple
import Data.ByteString.Char8 as BS (readInteger, unpack, null, pack)
import Data.Pool (Pool, withResource)
import qualified Data.List.Safe as Safe
import Crypto.BCrypt (validatePassword, hashPasswordUsingPolicy, fastBcryptHashingPolicy)
import Control.Monad (void, join)
import Control.Exception (throw, Exception)
import qualified Control.Monad.Catch as E (catch)
import Data.Maybe (fromMaybe)
import qualified Dsl.User as U (User(..), Role(..))
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.ToField (ToField (toField), Action (Escape))
import Data.Aeson (FromJSON, decode)
import Web.Scotty (ScottyM, post, body, status)
import Network.HTTP.Types (status400)
import Network.HTTP.Types.Status (status200)
import Control.Monad.IO.Class (MonadIO(liftIO))


existsTokenQuery :: Query
existsTokenQuery = "select count(*) from credentials where cred_type = 'TOKEN' and user_id = ? and cred_value = ? and expires_at > now();"


existsTokenForUserId :: Connection -> Integer -> String -> IO Bool
existsTokenForUserId conn userId token = let queryResult = (query conn existsTokenQuery (userId, token) :: IO [Only Int]) in
                                            fmap ((> 0) . fromOnly . head) queryResult


checkToken :: Pool Connection -> CheckCreds
checkToken pool userIdBytes tokenBytes = 
    let userIdParsed = readInteger userIdBytes
        token = unpack tokenBytes :: String
        userIdMaybe = userIdParsed >>= (\(userId, restString) -> if BS.null restString then Just userId else Nothing) in
    withResource pool 
        (\conn -> (pure userIdMaybe :: IO (Maybe Integer)) >>= maybe (pure False)  (\userId -> existsTokenForUserId conn userId token))


getPasswordByUserEmailQuery :: Query
getPasswordByUserEmailQuery = "select cred_value from credentials join coffee_user on coffee_user.id = credentials.user_id \
                                \where cred_type = 'PASSWORD' and coffee_user.email = ?;"


getPasswordByUserEmail :: Connection -> String -> IO (Maybe String)
getPasswordByUserEmail conn userEmail = let queryResult = query conn getPasswordByUserEmailQuery $ Only userEmail :: IO [String] in
                                        fmap Safe.head queryResult


validateUserPassword :: Connection -> String -> String -> IO Bool
validateUserPassword conn userEmail passwordAttempt = do
    passwordMaybe <- getPasswordByUserEmail conn userEmail
    let passwordBSMaybe = fmap pack passwordMaybe
    let passwordAttemptBS = pack passwordAttempt
    return $ maybe False (`validatePassword` passwordAttemptBS) passwordBSMaybe


data PasswordHashingException = PasswordHashingException deriving (Show)
data CreatingUserException = CreatingUserException deriving (Show)

instance Exception PasswordHashingException
instance Exception CreatingUserException


addPasswordForUserQuery :: Query
addPasswordForUserQuery = "INSERT INTO credentials (user_id, cred_type, cred_value) SELECT ?, 'PASSWORD', ? WHERE NOT EXISTS (SELECT 1 FROM credentials WHERE user_id = ?);"


addPasswordForUser :: Connection -> Integer -> String -> IO ()
addPasswordForUser conn userId userPassword = 
    let encodedPasswordIO = hashPasswordUsingPolicy fastBcryptHashingPolicy $ pack userPassword in
        do 
            encodedPasswordMaybe <- encodedPasswordIO
            let encodedPassword = fromMaybe (throw PasswordHashingException) encodedPasswordMaybe
            void $ execute conn addPasswordForUserQuery (userId, encodedPassword, userId)


createUserQuery :: Query
createUserQuery = "INSERT INTO coffee_user (user_role, user_name, user_last_name, phone, email) VALUES (?, ?, ?, ?, ?) RETURNING id;"

instance ToField U.Role where
  toField :: U.Role -> Action
  toField role = case role of
    U.UserRole         -> Escape . pack $ "USER"
    U.ManagerRole      -> Escape . pack $ "MANAGER"
    U.OrganizationRole -> Escape . pack $ "ORGANIZATION"

instance ToRow U.User

createUser :: Connection -> U.User -> String -> IO ()
createUser conn userDto userPassword = (returning conn createUserQuery [userDto] :: IO [Only Integer]) >>= 
    (let userId l = fromOnly $ fromMaybe (throw PasswordHashingException) $ Safe.head l in 
        \userIds -> addPasswordForUser conn (userId userIds) userPassword)

data RegistrationForm = RegistrationForm {
    name     :: String,
    lastName :: String,
    phone    :: Maybe String,
    email    :: String,
    password :: String
} deriving (Generic, Show)

instance FromJSON RegistrationForm

registrationFormToUser :: RegistrationForm -> U.User
registrationFormToUser form = U.User {
    U.role = U.UserRole,
    U.name = name form,
    U.lastName = lastName form,
    U.phone = phone form,
    U.email = email form
}

registrationRoute :: Pool Connection -> ScottyM ()
registrationRoute pool = post "/auth/register" $ do
    b <- body
    _ <- liftIO $ print b
    let formMaybe = decode b :: Maybe RegistrationForm
    _ <- liftIO $ print formMaybe
    let createUserWithConnection = withResource pool $ \conn -> return $ createUser conn
    let saveResult = fmap (\form -> join $ createUserWithConnection <*> pure (registrationFormToUser form) <*> pure (password form)) formMaybe
    let result = maybe (status status400) (\io -> liftIO io >> status status200) saveResult
    E.catch result (\PasswordHashingException -> status status400) >> E.catch result (\CreatingUserException -> status status400)
