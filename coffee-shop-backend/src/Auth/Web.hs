{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Auth.Web where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import Network.Wai.Middleware.HttpAuth (CheckCreds)
import Data.ByteString.Char8 as BS (readInteger, unpack, null)
import Auth.Service as Service (checkToken, PasswordHashingException (PasswordHashingException), CreatingUserException (CreatingUserException), createUser, checkPasswordAndCreateToken, InvalidCredentialsException (InvalidCredentialsException), getUserByToken)
import Web.Scotty (ScottyM, post, get, body, status, text, json, ActionM, jsonData, header, rescue)
import Auth.Model.RegistrationRequest (RegistrationForm (password), registrationFormToUser)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Control.Monad (join)
import Network.HTTP.Types (status400, status200)
import Control.Monad.Catch (catch, catches, Handler(..))
import Network.Wai (Request (pathInfo))
import qualified Auth.Model.LoginRequest as LoginRequest (LoginRequest(LoginRequest, email, password))
import Data.Maybe (fromMaybe)
import Control.Exception (Exception, throw)
import Auth.Model.LoginResponse (LoginResponse(LoginResponse, token))
import Network.HTTP.Types.Status (status403)
import Auth.Model.CheckTokenResponse (userToCheckTokenResponse)
import qualified Data.Text.Lazy as T (unpack)
import Auth.Db (NoSuchUserException(NoSuchUserException))


data MissedTokenException = MissedTokenException deriving (Show)

instance Exception MissedTokenException

registrationRoute :: Pool Connection -> ScottyM ()
registrationRoute pool = post "/auth/register" $ do
    b <- body
    _ <- liftIO $ print b
    let formMaybe = decode b :: Maybe RegistrationForm
    _ <- liftIO $ print formMaybe
    let createUserWithConnection userDto userPassword = withResource pool $ \conn -> return $ createUser conn userDto userPassword
    let saveResult = fmap (\form -> join $ createUserWithConnection (registrationFormToUser form) (password form)) formMaybe
    let result = maybe (status status400) (\io -> liftIO io >> status status200) saveResult
    catch result (\PasswordHashingException -> status status400) >> catch result (\CreatingUserException -> status status400)


signUpRoute :: Pool Connection -> ScottyM ()
signUpRoute pool = post "/auth/getToken" $ do
    form <- jsonData :: ActionM LoginRequest.LoginRequest
    _ <- liftIO $ print form
    let userEmail = LoginRequest.email form
    let userPassword = LoginRequest.password form
    token <- liftIO $ withResource pool $ \conn -> checkPasswordAndCreateToken conn userEmail userPassword
    let result = json (LoginResponse token)
    catch result (\InvalidCredentialsException -> status status403)


checkTokenRoute :: Pool Connection -> ScottyM ()
checkTokenRoute pool = get "/auth/checkToken" $ do
    tokenText <- header "X-Auth-Token"
    let token = maybe (throw MissedTokenException) (T.unpack) tokenText
    userRecord <- liftIO $ withResource pool $ \conn -> getUserByToken conn token
    let response = json $ userToCheckTokenResponse userRecord
    catches response [Handler (\(e :: MissedTokenException) -> status status400),
                      Handler (\(e :: NoSuchUserException) -> status status403)]
