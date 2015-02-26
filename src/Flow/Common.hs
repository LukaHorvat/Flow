{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Flow.Common
     ( module Imports
     , Diff(..)
     , with
     , apply ) where

import Control.Monad as Imports
import Control.Applicative as Imports
import Control.Arrow as Imports

class Diff a b where
    commit :: a -> b -> a

instance Diff [a] a where
    commit = flip (:)

instance Diff a ((->) a a) where
    commit = flip ($)

with :: Diff a b => a -> b -> a
with = commit

apply :: Diff a b => a -> [b] -> a
apply = foldl commit