import Data.Maybe

data RatNum = RatNum { 
                numerator    :: Int
               ,denominator  :: Int }

newtype MixedForm w r = (Maybe Int, Maybe RatNum)

instance Show MixedForm where
    show (Nothing, Nothing)     = 0
    show (Nothing, Just rat)    = show (numerator rat) ++ "/" ++ show (denominator rat)
    show (Just whole, Nothing)  = show (fromJust whole)
    show (Just whole, Just rat) = show (fromJust whole) ++ " and " ++ show (numerator (fromJust rat)) ++ "/" ++ show (denominator (fromJust rat))

instance Show RatNum where
    show rat = show (numerator rat) ++ "/" ++ show (denominator rat)

lowestForm :: RatNum -> RatNum
lowestForm rat = makeRat numer denom
    where divisor = gcd (numerator rat) (denominator rat)
          numer = (numerator rat) `quot` divisor
          denom = (denominator rat) `quot` divisor

makeRat :: Int -> Int -> RatNum
makeRat _ 0 = error "Rational numbers cannot have a zero denominator"
makeRat x y = RatNum { numerator = x, denominator = y }

(/+) :: RatNum -> RatNum -> RatNum
(/+) x y = makeRat numer denom
    where nx = numerator x
          dx = denominator x
          ny = numerator y
          dy = denominator y
          numer = nx * dy + dx * ny
          denom = dx * dy

(/*) :: RatNum -> RatNum -> RatNum
(/*) x y = makeRat numer denom
    where nx = numerator x
          dx = denominator x
          ny = numerator y
          dy = denominator y
          numer = nx * ny
          denom = dx * dy

(/-) :: RatNum -> RatNum -> RatNum
(/-) x y = makeRat numer denom
    where nx = numerator x
          dx = denominator x
          ny = numerator y
          dy = denominator y
          numer = nx * dy - dx * ny
          denom = dx * dy

(//) :: RatNum -> RatNum -> RatNum
(//) x y = x /* newy
    where newn = denominator y
          newd = numerator y
          newy = makeRat newn newd 

half = makeRat 1 2
third = makeRat 1 3
quarter = makeRat 1 4
fifth = makeRat 1 5


mixedForm :: RatNum -> MixedForm
mixedForm rat
    | d == n = (Just 1, Nothing)
    | d > n = (Nothing, Just $ lowestForm rat)
    | otherwise = (Just $ quot n d, Just $ lowestForm (makeRat (mod n d) d))
        where n = numerator rat
              d = denominator rat
    
    
    
    
    
    