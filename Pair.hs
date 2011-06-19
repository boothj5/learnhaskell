module Pair  
( pair
, first
, second
) where

data Pair a = Pair { getPair :: Int -> a }

instance Show a => Show (Pair a) where
    show p = show (first p) ++ " and " ++ show (second p)

pair :: a -> a -> Pair a
pair x y = Pair { getPair = (\f s pick -> if pick == 1 then f else s) x y }
                         
first :: Pair a -> a
first p = getPair p 1

second :: Pair a -> a
second p = getPair p 2
