import Data.List  
import Control.Applicative
  
-- -----------------------------------------
-- different approaches to calling functions
-- -----------------------------------------

-- Using the usual pattern matching and standard (left) associativity
numUniques1 :: (Eq a) => [a] -> Int  
numUniques1 xs = length (nub xs)

-- Using function application (right associative)
numUniques2 :: (Eq a) => [a] -> Int  
numUniques2 xs = length $ nub xs

-- Using function composition
numUniques3 :: (Eq a) => [a] -> Int  
numUniques3 = length . nub

-- Using a lambda function
numUniques4 :: (Eq a) => [a] -> Int  
numUniques4 = \xs -> length (nub xs)

-- Using a lambda function that uses function application
numUniques5 :: (Eq a) => [a] -> Int  
numUniques5 = \xs -> length $ nub xs

-- Combining function composition and function application
-- The associaticivity below is
--      nub is first combined with length (left) 
--          the result is a function that expects as list
--      xs is passed to this new function composition by applying xs (right)
numUniques6 :: (Eq a) => [a] -> Int  
numUniques6 = \xs -> length . nub $ xs

-- Foobar problem with guards, recursion and pattern matching
printFooBar x | (x `mod` 3 == 0) && (x `mod` 5 == 0) = "foobar"
			  | (x `mod` 3 == 0) 					 = "foo"
			  | (x `mod` 5 == 0) 					 = "bar"
			  | otherwise							 = show x

fooBar100 []     = []
fooBar100 (x:[]) = printFooBar x : []
fooBar100 (x:xs) = printFooBar x : fooBar100 xs

-- Foobar problem using list comprehension 
fooBarBetter xs = [ if ((x `mod` 3 == 0) && (x `mod` 5 == 0)) then "foobar" 
					else if (x `mod` 3 == 0) then "foo"
					else if (x `mod` 5 == 0) then "bar"
					else show x
					| x <- xs , x <= 100 ]

-- More list comprehension examples
triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]  
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]  

-- quicksort using let and list comprehension
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

-- --------------------------------------------------------------------
-- Different appraoches to finding a key in an association list [(k,v)]
-- Just for illustration, this is all in Data.Map
-- --------------------------------------------------------------------

phonebook = [("James", "07747777468"), ("Home", "01473749749")]

-- compose snd (second in tuple), head (first in list) and filter (using a lambda
-- predicate to return those that match)
-- This composed function will take a list so pass it using function application
-- This implementation will result in exception if key not present (cannot call head on empty list)
findKey1 :: (Eq k) => k -> [(k,v)] -> v
findKey1 key xs = snd . head . filter (\(k,v) -> key == k) $ xs

-- Exactly the same, but returns a partial function (which requires a list) instead
findKey2 :: (Eq k) => k -> [(k,v)] -> v
findKey2 key = snd . head . filter (\(k,v) -> key == k)

-- Use Maybe data type so we can return nothing, or one element
findKey3 :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey3 key [] = Nothing  
findKey3 key ((k,v):xs) = if key == k  
                            then Just v  
                            else findKey3 key xs

-- Use a fold such a common pattern
-- Note, we're not passing the list, so again we return a partial function that will
-- take a list as a paramater
findKey4 :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey4 key = foldl (\acc (k,v) -> if key == k then Just v else acc) Nothing

-- --------------------------
-- Higher order functions
-- Type classes, functors etc
-- --------------------------

myAdd2 :: (Num a) => a -> a
myAdd2 = (+) 2

myAdd2AndShow :: (Num a) => a -> String
myAdd2AndShow = show . myAdd2

-- Map is an fmap over lists
-- fmap :: (a -> b) -> f a -> f b
--  takes 
--      function of type a to type b
--      type contrucur that takes type a as parameter (think Maybe)
--      returns type contructor that takes type b as parameter
-- map :: (a -> b) -> [a] -> [b]
--  takes
--      function of type a to type b
--      type contructor of [] a
--      return types constructor of [] b

-- map myAdd2 [1,2,3,4]
-- map show [1,2,3,4]

-- Types that can act like boxes can be instances of Functor
-- Lists, Maybe, LinkedList (below)
--
-- fmap myAdd2 $ Just 3
-- fmap show $ Just 3


