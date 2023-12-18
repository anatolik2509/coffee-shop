module Auth.Service where

import Database.PostgreSQL.Simple (Connection)
import Crypto.BCrypt (validatePassword, hashPasswordUsingPolicy, fastBcryptHashingPolicy)
import Auth.Db (addPasswordForUser, saveUser, getPasswordByUserEmail, existsTokenForUserId, getUserIdByEmail, addToken, findUserByToken)
import Data.Maybe (fromMaybe)
import Control.Exception (throw, Exception)
import Data.ByteString.Char8 as BS (pack)
import qualified Dsl.User as U
import Auth.Model.RegistrationRequest (RegistrationForm(email))
import Data.UUID.V4
import Data.UUID
import Dsl.User (User)


data PasswordHashingException = PasswordHashingException deriving (Show)
data CreatingUserException = CreatingUserException deriving (Show)
data InvalidCredentialsException = InvalidCredentialsException deriving (Show)


instance Exception PasswordHashingException
instance Exception CreatingUserException
instance Exception InvalidCredentialsException


encodeAndAddPasswordForUser :: Connection -> Integer -> String -> IO ()
encodeAndAddPasswordForUser conn userId userPassword =
    let encodedPasswordIO = hashPasswordUsingPolicy fastBcryptHashingPolicy $ pack userPassword in
        do
            encodedPasswordMaybe <- encodedPasswordIO
            let encodedPassword = fromMaybe (throw PasswordHashingException) encodedPasswordMaybe
            addPasswordForUser conn userId encodedPassword


createUser :: Connection -> U.User -> String -> IO ()
createUser conn userDto userPassword = do
    userId <- saveUser conn userDto
    encodeAndAddPasswordForUser conn userId userPassword


validateUserPassword :: Connection -> String -> String -> IO Bool
validateUserPassword conn userEmail passwordAttempt = do
    passwordMaybe <- getPasswordByUserEmail conn userEmail
    let passwordBSMaybe = fmap pack passwordMaybe
    let passwordAttemptBS = pack passwordAttempt
    return $ maybe False (`validatePassword` passwordAttemptBS) passwordBSMaybe


checkPasswordAndCreateToken :: Connection -> String -> String -> IO String
checkPasswordAndCreateToken conn userEmail passwordAttempt = do
    validateResult <- validateUserPassword conn userEmail passwordAttempt
    if validateResult then createToken conn userEmail else throw InvalidCredentialsException


createToken :: Connection -> String -> IO String
createToken conn userEmail = do
    userId <- getUserIdByEmail conn userEmail
    token <- fmap toString nextRandom
    _ <- print token
    _ <- addToken conn token userId
    return token


getUserByToken :: Connection -> String -> IO User
getUserByToken = findUserByToken

checkToken :: Connection -> Integer -> String -> IO Bool
checkToken = existsTokenForUserId
