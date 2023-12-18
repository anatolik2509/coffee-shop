{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Auth.Db where

import Database.PostgreSQL.Simple
import qualified Data.List.Safe as Safe
import Control.Monad (void, join)
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple.ToField (ToField (..), Action (..))
import qualified Dsl.User as U
import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple.FromField (FromField (fromField), conversionError, FieldParser)
import Dsl.User (Role)
import Database.PostgreSQL.Simple.FromRow (field, RowParser)
import Control.Exception (Exception, throw)
import Data.ByteString.Char8 (unpack)
import Data.Time (getCurrentTime, addUTCTime, secondsToDiffTime, secondsToNominalDiffTime, UTCTime (UTCTime))
import Dsl.KeyedRecord (KeyedRecord)
import Data.Maybe (fromMaybe)


existsTokenQuery :: Query
existsTokenQuery = "select count(*) from credentials where cred_type = 'TOKEN' and user_id = ? and cred_value = ? and expires_at > now();"

existsTokenForUserId :: Connection -> Integer -> String -> IO Bool
existsTokenForUserId conn userId token = let queryResult = (query conn existsTokenQuery (userId, token) :: IO [Only Int]) in
                                            fmap ((> 0) . fromOnly . head) queryResult


findUserByTokenQuery :: Query
findUserByTokenQuery = "select u.* from coffee_user u join credentials c on u.id = c.user_id where cred_type = 'TOKEN' and cred_value = ? and expires_at > now();"

findUserByToken :: Connection -> String -> IO U.User
findUserByToken conn token = let queryResult = query conn findUserByTokenQuery $ Only token :: IO [U.User] in
                                        fmap (fromMaybe (throw NoSuchUserException) . Safe.head) queryResult


getPasswordByUserEmailQuery :: Query
getPasswordByUserEmailQuery = "select cred_value from credentials join coffee_user on coffee_user.id = credentials.user_id \
                                \where cred_type = 'PASSWORD' and coffee_user.email = ?;"

getPasswordByUserEmail :: Connection -> String -> IO (Maybe String)
getPasswordByUserEmail conn userEmail = let queryResult = query conn getPasswordByUserEmailQuery $ Only userEmail :: IO [Only String] in
                                        fmap (fmap fromOnly . Safe.head) queryResult


addPasswordForUserQuery :: Query
addPasswordForUserQuery = "INSERT INTO credentials (user_id, cred_type, cred_value) SELECT ?, 'PASSWORD', ? WHERE NOT EXISTS (SELECT 1 FROM credentials WHERE user_id = ?);"

addPasswordForUser :: Connection -> Integer -> ByteString -> IO ()
addPasswordForUser conn userId encodedPassword =
    void $ execute conn addPasswordForUserQuery (userId, encodedPassword, userId)


saveUserQuery :: Query
saveUserQuery = "INSERT INTO coffee_user (user_role, user_name, user_last_name, phone, email) VALUES (?, ?, ?, ?, ?) RETURNING id;"

saveUser :: Connection -> U.User -> IO Integer
saveUser conn userDto = fromOnly . head <$> (returning conn saveUserQuery [userDto] :: IO [Only Integer])


addTokenQuery :: Query
addTokenQuery = "INSERT INTO credentials (user_id, cred_type, cred_value, expires_at) VALUES (?, 'TOKEN', ?, ?);"

addToken :: Connection -> String -> Integer -> IO Integer
addToken conn token userId = do
  expireAt <- fmap (addUTCTime $ secondsToNominalDiffTime 3600) getCurrentTime
  rows <- execute conn addTokenQuery (userId, token, expireAt)
  return $ fromIntegral rows


getUserIdByEmailQuery :: Query
getUserIdByEmailQuery = "SELECT id FROM coffee_user WHERE email = ?"

getUserIdByEmail :: Connection -> String -> IO Integer
getUserIdByEmail conn userEmail =
  let queryResult = query conn getUserIdByEmailQuery $ Only userEmail :: IO [Only Integer]
      queriedUser = fmap Safe.head queryResult :: IO (Maybe (Only Integer))
  in fmap (fromOnly . fromMaybe (throw NoSuchUserException)) queriedUser


data RoleConversionException = RoleConversionException deriving (Show)
data NoSuchUserException = NoSuchUserException deriving (Show)

instance Exception RoleConversionException
instance Exception NoSuchUserException

instance ToRow U.User

instance ToField U.Role where
  toField :: U.Role -> Action
  toField role = case role of
    U.UserRole         -> Escape . pack $ "USER"
    U.ManagerRole      -> Escape . pack $ "MANAGER"
    U.OrganizationRole -> Escape . pack $ "ORGANIZATION"

instance FromRow U.User

instance FromField U.Role where
  fromField :: FieldParser Role
  fromField _ valueBytes =
    let roleValueStrMaybe = fmap unpack valueBytes
        roleValue roleValueStr = case roleValueStr of
          "USER" ->         return U.UserRole
          "MANAGER" ->      return U.ManagerRole
          "ORGANIZATION" -> return U.OrganizationRole
          _ ->              conversionError RoleConversionException
    in maybe (conversionError RoleConversionException) roleValue roleValueStrMaybe
