{-# LANGUAGE InstanceSigs #-}
module Dsl.KeyedRecord(KeyedRecord(..)) where

data KeyedRecord k r = Record k r | Key k

instance Functor (KeyedRecord k) where  
    fmap :: (a -> b) -> KeyedRecord k a -> KeyedRecord k b
    fmap f kr = case kr of
        Record key val -> Record key (f val)
        Key key        -> Key key 
