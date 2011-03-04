import Control.Applicative
import Control.Monad.Writer
import Data.Monoid

data SomethingContext a = Something a
    deriving (Show)

instance Functor SomethingContext where
    fmap f (Something x) = Something (f x)

instance Applicative SomethingContext where
    pure x = Something x
    (Something f) <*> (Something x) = Something (f x)

instance Num a => Monoid (SomethingContext a) where
    mempty = Something 0
    mappend j1 j2 = addSomething j1 j2

instance Monad SomethingContext where
    return x = Something x
    Something x >>= f = f x

addSomething :: (Num a) => SomethingContext a -> SomethingContext a -> SomethingContext a
addSomething (Something x) (Something y) = Something (x + y)

myFunction :: (Num n) => n -> n -> n -> n
myFunction x y z = x * y + z

myFuncOnSomething :: (Num n) => SomethingContext n -> SomethingContext n -> SomethingContext n -> SomethingContext n
myFuncOnSomething j1 j2 j3 = do
    x <- j1
    y <- j2
    z <- j3
    Something $ myFunction x y z

multThree :: (Num n) => SomethingContext n -> SomethingContext n -> SomethingContext n -> SomethingContext n
multThree s1 s2 s3 = do 
    x <- s1 
    y <- s2  
    z <- s3  
    return (x*y*z)

multThreeIgnoreSecond :: (Num n) => SomethingContext n -> SomethingContext n -> SomethingContext n -> SomethingContext n
multThreeIgnoreSecond s1 s2 s3 = do 
    x <- s1 
    s2  
    z <- s3  
    return (x*z)

logSomething :: SomethingContext Int -> Writer [String] (SomethingContext Int)
logSomething (Something n) = Writer ((Something n), ["Received " ++ show n])  

addSomethings :: SomethingContext Int -> SomethingContext Int -> SomethingContext Int -> Writer [String] (SomethingContext Int)
addSomethings s1 s2 s3 = do
    x <- logSomething s1
    y <- logSomething s2
    z <- logSomething s3
    return (x `mappend` y `mappend` z)

accSomething :: SomethingContext Integer -> Writer (SomethingContext Integer) ()
accSomething (Something n) = Writer ((), (Something n))

accSomethings :: SomethingContext Integer -> SomethingContext Integer -> SomethingContext Integer -> Writer (SomethingContext Integer) ()
accSomethings s1 s2 s3 = do
    x <- accSomething s1
    y <- accSomething s2
    z <- accSomething s3
    return ()

main1 = 
    getLine >>= (\first -> 
    putStrLn ("first thing was: " ++ first) >>= (\_ -> 
    getLine >>= (\second -> 
    putStrLn ("second thing was: " ++ second) >>= (\_ ->
    fmap read getLine >>= (\number ->
    if number > 10 
        then putStrLn ("Bigger") 
        else putStrLn ("Smaller"))))))

main2 = 
    getLine >>= (\first -> 
    putStrLn ("first thing was: " ++ first) >> ( 
    getLine >>= (\second -> 
    putStrLn ("second thing was: " ++ second) >> (
    fmap read getLine >>= (\number ->
    if number > 10 
        then putStrLn ("Bigger") 
        else putStrLn ("Smaller"))))))

main3 = 
    getLine >>= \first -> 
    putStrLn ("first thing was: " ++ first) >>  
    getLine >>= \second -> 
    putStrLn ("you said: " ++ second) >> 
    fmap read getLine >>= \number ->
    if number > 10 
        then putStrLn ("Bigger") 
        else putStrLn ("Smaller")

main4 = do
    first <- getLine
    putStrLn ("first thing was: " ++ first)
    second <- getLine
    putStrLn ("you said: " ++ second)
    number <- fmap read getLine
    if number > 10
        then putStrLn ("Bigger")
        else putStrLn ("Smaller")
        
doubleWithLog :: (Num a) => SomethingContext a -> Writer String (SomethingContext a)
doubleWithLog (Something x) = Writer (Something $ x*2, "Doubled " ++ show (Something x) ++ ", ")

addDoubles :: (Num a) => SomethingContext a -> SomethingContext a -> SomethingContext a -> Writer String (SomethingContext a)
addDoubles s1 s2 s3 = do
    x <- doubleWithLog s1
    y <- doubleWithLog s2
    z <- doubleWithLog s3
    tell "Adding the doubles."
    return (addSomething (addSomething x y) z)

addDoublesAlt :: (Num a) => SomethingContext a -> SomethingContext a -> SomethingContext a -> Writer String (SomethingContext a)
addDoublesAlt s1 s2 s3= 
    doubleWithLog s1 >>= (\x ->
    doubleWithLog s2 >>= (\y ->
    doubleWithLog s3 >>= (\z ->
    tell "Adding the doubled" >> (
    return (addSomething (addSomething x y) z)))))
