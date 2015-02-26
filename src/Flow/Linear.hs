{-# LANGUAGE DeriveGeneric #-}
module Flow.Linear where

import Data.Serialize
import GHC.Generics

data Vec2 a = Vec2 a a deriving (Show, Generic, Eq, Ord)

instance Serialize a => Serialize (Vec2 a)

instance Num a => Num (Vec2 a) where
    (Vec2 x y) + (Vec2 z w) = Vec2 (x + z) (y + w)
    (Vec2 x y) * (Vec2 z w) = Vec2 (x * z) (y * w)
    negate (Vec2 x y) = Vec2 (-x) (-y)
    abs (Vec2 x y) = Vec2 (abs x) (abs y)
    signum (Vec2 x y) = Vec2 (signum x) (signum y)
    fromInteger x = Vec2 (fromInteger x) (fromInteger x)
