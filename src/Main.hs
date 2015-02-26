{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.ByteString (ByteString, foldl)
import Data.Serialize
import GHC.Generics

data Test a b = Choice1 a | Choice2 b deriving (Show, Generic)

instance (Serialize a, Serialize b) => Serialize (Test a b)

a :: [Int]
a = [1,2,3,45,5] ++ a

showBS :: ByteString -> String
showBS = (show :: [Int] -> String) . Data.ByteString.foldl (\l w -> fromIntegral w : l) []

main = do
    let code = encode a
    print $ showBS code
    print (decode code :: Either String [Int])