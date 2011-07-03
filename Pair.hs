module Pair  
( pair
, first
, second
) where

import Data.Either

data Pair a b = Pair { getPair :: Int -> [Either a b] } | Nil

instance (Show a, Show b) => Show (Pair a b) where
    show Nil = "Empty"
    show p = show (first p) ++ " and " ++ show (second p)

pick :: a -> b -> Int -> [Either a b]
pick x y p | p == 1    = [Left x]
           | otherwise = [Right y]

pair :: a -> b -> Pair a b
pair x y = Pair { getPair = pick x y }
                         
first :: Pair a b -> a
first p = head $ lefts $ getPair p 1

second :: Pair a b -> b
second p = head $ rights $ getPair p 2


