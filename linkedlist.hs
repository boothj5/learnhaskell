import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State
import Data.Monoid
import qualified Data.Foldable as F
  
-- types and typeclass instances

data LinkedList a = EmptyList | Node a (LinkedList a)
    deriving (Show)

newtype ConcList a = ConcList { getConcList :: LinkedList a }
    deriving (Show)

newtype CombList a = CombList { getCombList :: LinkedList a } 
    deriving (Show)

instance Functor LinkedList where
    fmap f EmptyList          = EmptyList
    fmap f (Node item (rest)) = Node (f item) (fmap f (rest))

instance Applicative LinkedList where
    pure n = Node n (pure n)
    EmptyList <*> _ = EmptyList
    _ <*> EmptyList = EmptyList
    (Node f (r1)) <*> (Node n (r2)) = Node (f n) (r1 <*> r2)
--    (Node n (r1)) <*> (Node f (r2)) = Node (f n) (r1 <*> r2)

instance Monoid (ConcList a) where
    mempty = ConcList EmptyList
    mappend l (ConcList EmptyList) = l
    mappend (ConcList EmptyList) l = l
    mappend (ConcList (Node n (EmptyList))) l2 = ConcList (Node n (getConcList l2))
    mappend (ConcList (Node n r1)) l2 = ConcList (Node n (getConcList (((ConcList r1) `mappend` l2))))

instance (Ord a) => Monoid (CombList a) where
    mempty = CombList EmptyList
    mappend l (CombList EmptyList) = l
    mappend (CombList EmptyList) l = l
    mappend (CombList (Node n EmptyList)) l2 = CombList (listInsert n (getCombList l2))
    mappend (CombList (Node n r1)) l2 = CombList r1 `mappend` CombList (listInsert n (getCombList l2))

instance F.Foldable LinkedList where
    foldMap f EmptyList = mempty
    foldMap f (Node n rest) = f n `mappend` F.foldMap f rest

instance Monad LinkedList where
   return x = Node x EmptyList
   list >>= f = listConcatAll $ fmap f list
   fail _ = EmptyList
    
-- LinkedList functions
listInsert :: (Ord a) => a -> LinkedList a -> LinkedList a
listInsert item EmptyList = Node item (EmptyList)
listInsert item (Node elem (rest)) 
    | item <= elem = Node item (Node elem (rest))
    | item > elem  = Node elem (listInsert item rest)

listInsertST :: (Ord a) => a -> State (LinkedList a) ()
listInsertST item = State $ \list -> ((), listInsert item list)

listRemoveST :: (Integral n) => n -> State (LinkedList a) ()
listRemoveST index = State $ \list -> ((), listRemove index list)

listGet :: (Integral n) => n -> LinkedList a -> Maybe a
listGet index EmptyList          = Nothing 
listGet 0 (Node elem _)          = Just elem
listGet index (Node elem (rest)) = (listGet (index - 1) rest)

listSize :: LinkedList a -> Int
listSize EmptyList          = 0 
listSize (Node n EmptyList) = 1
listSize (Node n rest)      = 1 + listSize rest

listRemove :: (Integral n) => n -> LinkedList a -> LinkedList a
listRemove _ EmptyList          = EmptyList
listRemove 0 (Node n rest)      = rest
listRemove index (Node n rest)  = Node n (listRemove (index -1) rest)

listConcat :: LinkedList a -> LinkedList a -> LinkedList a
listConcat EmptyList l = l
listConcat l EmptyList = l
listConcat (Node n1 r2) l2 = Node n1 (listConcat r2 l2)

listConcatAll :: LinkedList (LinkedList a) -> LinkedList a
listConcatAll EmptyList = EmptyList
listConcatAll (Node list1 EmptyList) = list1
listConcatAll (Node list1 (Node list2 rest)) = listConcatAll (Node (listConcat list1 list2) rest)

fromPrimitive :: [a] -> LinkedList a
fromPrimitive [] = EmptyList
fromPrimitive (x:[]) = Node x EmptyList
fromPrimitive (x:xs) = Node x (fromPrimitive xs)

toPrimitive :: LinkedList a -> [a]
toPrimitive EmptyList = []
toPrimitive (Node x EmptyList) = x:[]
toPrimitive (Node x xs) = x:(toPrimitive xs)

-- Tests
myList = Node 1 (Node 2 (Node 3 (Node 4 (Node 5 (Node 6 (Node 7 (Node 8 (Node 9 (Node 10 EmptyList)))))))))

newList = fmap (\value -> value * 100) myList

list1To10 = myList
list10To100 = fmap (*10) list1To10

biggerThan2 = fmap (>2) myList

ifDivBy3Add2 :: (Integral t) => t -> t
ifDivBy3Add2 n | (rem n 3 /= 0) = n
               | otherwise      = n + 2

anotherList = fmap ifDivBy3Add2 myList

functionList = fmap (*) myList
mappedFunctionList = fmap (\value -> value 2) functionList
applicativeMappedList = functionList <*> myList
addedLists = pure (+) <*> myList <*> applicativeMappedList
alternativeAddedLists = (+) <$> myList <*> applicativeMappedList
threeAddedLists = (+) <$> ((+) <$> myList <*> applicativeMappedList) <*> newList

orderList = Node "First:" (Node "Second:" (Node "Third:" (Node "Fourth:" EmptyList)))
someStrList = Node "James" (Node "Steve" (Node "Dave" (Node "Mike" EmptyList)))
concatList = (++) <$> orderList <*> someStrList

firstList = Node 2 (Node 3 EmptyList)
secondList = Node 10 (Node 12 EmptyList)
appConcatList = getConcList $ ConcList firstList `mappend` ConcList secondList
concatThreeList = getConcList $ ConcList firstList `mappend` ConcList secondList `mappend` ConcList firstList

comb1 = Node 1 (Node 3 (Node 6 EmptyList))
comb2 = Node 2 (Node 7 (Node 8 EmptyList))
combined = getCombList $ CombList comb1 `mappend` CombList comb2

added = F.foldl (+) 0 myList

treeList = Node comb1 (Node comb2 (Node myList EmptyList))

multiplyAllElements :: (Num a) => LinkedList a -> LinkedList a -> LinkedList a
multiplyAllElements list1 list2 = list1 >>= (\x -> 
                                  list2 >>= (\y -> 
                                  Node (x*y) EmptyList))

multiplyAllElementsDo :: (Num a) => LinkedList a -> LinkedList a -> LinkedList a
multiplyAllElementsDo list1 list2 = do
    x <- list1
    y <- list2
    Node (x*y) EmptyList
    

processLinkedList :: LinkedList Char -> LinkedList Char
processLinkedList list = let
    newList1 = listInsert 'f' list
    newList2 = listRemove 0 newList1
    newList3 = listInsert 'a' newList2
    in newList3

processLinkedListST :: State (LinkedList Char) ()
processLinkedListST = do 
    listInsertST 'f' 
    listRemoveST 0 
    listInsertST 'a' 

main = do
    putStrLn "myList"
    putStrLn $ show myList
    putStrLn ""
    putStrLn "Elements 0, 5 and 10"
    putStrLn $ show $ listGet 0 myList
    putStrLn $ show $ listGet 5 myList
    putStrLn $ show $ listGet 10 myList
    putStrLn ""
 
    putStrLn "newList = fmap (\\value -> value * 100) myList"
    putStrLn $ show newList
    putStrLn ""

    putStrLn "biggerThan2 = fmap (>2) myList"
    putStrLn $ show biggerThan2
    putStrLn ""
 
    putStrLn "anotherList = fmap ifDivBy3Add2 myList"
    putStrLn $ show anotherList
    putStrLn ""
 
    putStrLn "functionList = fmap (*) myList"
    putStrLn "mappedFunctionList = fmap (\\value -> value 2) functionList"
    putStrLn $ show mappedFunctionList
    putStrLn ""
 
    putStrLn "applicativeMappedList = functionList <*> myList"
    putStrLn $ show applicativeMappedList
    putStrLn ""
 
    putStrLn "addedLists = pure (+) <*> myList <*> applicativeMappedList"
    putStrLn $ show addedLists
    putStrLn ""
 
    putStrLn "alternativeAddedLists = (+) <$> myList <*> applicativeMappedList"
    putStrLn $ show alternativeAddedLists
    putStrLn ""
 
    putStrLn "threeAddedLists = (+) <$> ((+) <$> myList <*> applicativeMappedList) <*> newList"
    putStrLn $ show threeAddedLists
    putStrLn ""
    
    putStrLn "orderList:"
    putStrLn $ show orderList
    putStrLn "someStrList:"
    putStrLn $ show someStrList
    putStrLn "concatList = (++) <$> orderList <*> someStrList"
    putStrLn $ show concatList
    putStrLn ""
 
    putStrLn "firstList:"
    putStrLn $ show firstList
    putStrLn "secondList:"
    putStrLn $ show secondList
    putStrLn "getConcList $ ConcList firstList `mappend` ConcList secondList"
    putStrLn $ show appConcatList
    putStrLn "getConcList $ ConcList firstList 'mappend' ConcList secondList `mappend` ConcList firstList"
    putStrLn $ show concatThreeList
    putStrLn ""

    putStrLn "comb1:"
    putStrLn $ show comb1
    putStrLn "comb2:"
    putStrLn $ show comb2
    putStrLn $ "combined = getCombList $ CombList comb1 `mappend` CombList comb2"
    putStrLn $ show combined
    putStrLn ""

    putStrLn "added = F.foldl (+) 0 myList"
    putStrLn $ show added


