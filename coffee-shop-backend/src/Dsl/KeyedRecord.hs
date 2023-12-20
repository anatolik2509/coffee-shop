{-# LANGUAGE InstanceSigs #-}
module Dsl.KeyedRecord(KeyedRecord(..), getKey, toTuple, getValue) where

data KeyedRecord k r = Record k r | Key k

instance Functor (KeyedRecord k) where  
    fmap :: (a -> b) -> KeyedRecord k a -> KeyedRecord k b
    fmap f kr = case kr of
        Record key val -> Record key (f val)
        Key key        -> Key key 

getKey :: KeyedRecord k r -> k
getKey (Record k _) = k
getKey (Key k)      = k

getValue :: KeyedRecord k r -> Maybe r
getValue (Record k r) = Just r
getValue (Key k)      = Nothing

toTuple :: KeyedRecord k r -> (k, Maybe r)
toTuple (Record k r) = (k, Just r)
toTuple (Key k)      = (k, Nothing)
