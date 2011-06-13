data RatNum = RatNum { 
                numerator    :: Int
               ,denominator  :: Int }

instance Show RatNum where
    show rat = show (numerator ratL) ++ "/" ++ show (denominator ratL)
        where ratL = lowestForm rat

lowestForm :: RatNum -> RatNum
lowestForm rat = makeRat numer denom
    where divisor = gcd (numerator rat) (denominator rat)
          numer = (numerator rat) `quot` divisor
          denom = (denominator rat) `quot` divisor


makeRat :: Int -> Int -> RatNum
makeRat _ 0 = error "Rational numbers cannot have a zero denominator"
makeRat x y = RatNum { numerator = x, denominator = y }

(~+~) :: RatNum -> RatNum -> RatNum
(~+~) x y = RatNum { numerator = numer, denominator = denom }
    where nx = numerator x
          dx = denominator x
          ny = numerator y
          dy = denominator y
          numer = nx * dy + dx * ny
          denom = dx * dy

(~*~) :: RatNum -> RatNum -> RatNum
(~*~) x y = RatNum { numerator = numer, denominator = denom }
    where nx = numerator x
          dx = denominator x
          ny = numerator y
          dy = denominator y
          numer = nx * ny
          denom = dx * dy

