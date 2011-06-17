import Rat

half :: RatNum
half = makeRat 1 2

third :: RatNum
third = makeRat 1 3

quarter :: RatNum
quarter = makeRat 1 4

fifth :: RatNum
fifth = makeRat 1 5

main = do
    putStrLn $ show half
    putStrLn $ show third
    putStrLn $ show quarter
    putStrLn $ show fifth
