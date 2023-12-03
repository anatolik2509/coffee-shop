{-# LANGUAGE OverloadedStrings #-}

module Auth where

import Network.Wai.Middleware.HttpAuth
import Database.PostgreSQL.Simple
import Data.Pool

--checkToken :: CheckCreds
--checkToken _ token = 

getTokenQuery :: Query
getTokenQuery = "select count(*) from credentials where cred_type = 'TOKEN' and user_id = ? and cred_value = ?"

existsTokenForUserId :: Pool Connection -> Integer -> String -> IO Bool
existsTokenForUserId pool userId token = let q conn = (query conn getTokenQuery (userId, token) :: IO [Only Int]) in
                                        withResource pool $ fmap ((> 0) . fromOnly . head) . q