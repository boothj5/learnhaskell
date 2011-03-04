type Birds = Int  
type Pole = (Birds,Birds)  

landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing  

x -: f = f x


-- >>= 
-- will extract the value from the mondadic value (3 from Just 3)
-- apply the function which is its second argument and must return a monadic value
-- return the result mapped back to the monadic value
-- Maybe is monadic
-- show is just to emphaise the b in
-- >>= : m a -> (a -> m b) -> m b
added5 = Just 5 >>= (\x -> Just (show ((x + 2))))
addedSome = Just 5 >>= (\x -> 
            Just 3 >>= (\y -> 
            Just 1 >>= (\z -> 
            Just (show (x + y + z)))))

-- the above is a set of nested lambdas which are the function passed to the monad
-- the result of the final lambda is a monadic value
-- each lambda just binds the value from the mondic value
-- So:
-- glue the landas together with a do block
addedSomeWithDo :: Maybe String
addedSomeWithDo = do
    x <- Just 5
    y <- Just 3
    z <- Just 1
    Just (show (x + y + z))
    

-- y becomes "!", since it is extracted from the Just "!" Maybe monad
-- x become 3, since it is extracted from the Just 3 Maybe monad
-- essentially we're setting x to 3 and y to "!" before executing show x ++ y
myString = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))




