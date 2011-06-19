module Rat  
( RatNum
, makeRat
, makeMixedForm
, parseRat
) where

import Data.Maybe
import Data.Char
import GHC.Real

data RatNum = RatNum { 
                numer    :: Int
               ,denom  :: Int }

data MixedForm = MixedForm { whole    :: Maybe Int 
                           , rational :: Maybe RatNum }

instance Show MixedForm where
    show mf | isNothing (whole mf) && isNothing (rational mf) = "0"
            | isNothing (whole mf) = show (numer (fromJust (rational mf))) ++ "/" ++ show (denom (fromJust (rational mf)))
            | isNothing (rational mf) = show (fromJust $ whole mf)
            | otherwise = show (fromJust (whole mf)) 
                ++ " and " 
                ++ show (numer (fromJust (rational mf))) 
                ++ "/" ++ show (denom (fromJust (rational mf)))

instance Eq RatNum where
    a == b = numer al == numer bl && denom al == denom bl
        where al = lowestForm a
              bl = lowestForm b

instance Ord RatNum where
    a <= b = numer al <= numer bl
        where al = lowestForm a
              bl = lowestForm b  
     

instance Num RatNum where
    (+) = (/+)
    (-) = (/-)
    (*) = (/*)
    abs rat = makeRat (abs (numer rat)) (denom rat)
    fromInteger int 
        | int < 0   = makeRat (-1) 1
        | int == 0  = makeRat 0 1
        | otherwise = makeRat 1 1 
    signum rat 
        | (numer rat) < 0 && (denom rat) > 0 = (-1)
        | (numer rat) == 0 = 0
        | otherwise = 1

instance Fractional RatNum where
    (/) = (//)
    fromRational rat = makeRat ( fromInteger . numerator $ rat ) ( fromInteger . denominator $ rat)

instance Show RatNum where
    show rat = show (numer ratL) ++ "/" ++ show (denom ratL)
        where ratL = lowestForm rat

lowestForm :: RatNum -> RatNum
lowestForm rat = makeRat n d
    where divisor = gcd (numer rat) (denom rat)
          n = (numer rat) `quot` divisor
          d = (denom rat) `quot` divisor

makeRat :: Int -> Int -> RatNum
makeRat _ 0 = error "Rational numbers cannot have a zero denom"
makeRat x y = RatNum { numer = x, denom = y }

(/+) :: RatNum -> RatNum -> RatNum
(/+) x y = makeRat n d
    where n = (numer x) * (denom y) + (denom x) * (numer y)
          d = (denom x) * (denom y)

(/*) :: RatNum -> RatNum -> RatNum
(/*) x y = makeRat n d
    where n = (numer x) * (numer y)
          d = (denom x) * (denom y)

(/-) :: RatNum -> RatNum -> RatNum
(/-) x y = makeRat n d
    where n = (numer x) * (denom y) - (denom x) * (numer y)
          d = (denom x) * (denom y)

(//) :: RatNum -> RatNum -> RatNum
(//) x y = x /* newy
    where newn = denom y
          newd = numer y
          newy = makeRat newn newd 

half = makeRat 1 2
third = makeRat 1 3
quarter = makeRat 1 4
fifth = makeRat 1 5


makeMixedForm :: RatNum -> MixedForm
makeMixedForm rat
    | d == n    = MixedForm { whole    = Just 1
                            , rational = Nothing }
    | d > n     = MixedForm { whole    = Nothing
                            , rational = (Just (lowestForm rat)) }
    | otherwise = MixedForm { whole    = (Just (quot n d))
                            , rational = (Just (lowestForm (makeRat (mod n d) d)))  }
        where n = numer rat
              d = denom rat
    
parseRat :: String -> RatNum
parseRat str 
    | length splitList /= 2 = error "Invalid rational number!!!1!"
    | not . isDigit . head $ (splitList !! 0) = error "Invalid rational number!!!1!"
    | not . isDigit . head $ (splitList !! 1) = error "Invalid rational number!!!1!"
    | otherwise = makeRat (read (splitList !! 0)) (read (splitList !! 1))
        where splitList = (splitBy ((==) '/') str)


splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list = first : splitBy f (dropWhile f rest) 
    where (first, rest) = break f list  
    
    
    
