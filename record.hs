--
-- Without using record syntax
data Player = Player String String

firstName :: Player -> String
firstName (Player firstName _) = firstName

secondName :: Player -> String
secondName (Player _ secondName) = secondName

-- example usage
james :: Player
james = Player "James" "Bond"

--
-- Using record syntax
data PlayerRec = PlayerRec { rFirstName :: String
                            ,rSecondName :: String
                           }

-- example usage
jackie :: PlayerRec
jackie = PlayerRec "Jackie" "Chan"

bruce :: PlayerRec
bruce = PlayerRec { rFirstName = "Bruce", rSecondName = "Lee" }

-- fields can be in any order
chuck :: PlayerRec
chuck = PlayerRec { rSecondName = "Norris", rFirstName = "Chuck" } 

--
-- Takes type variable
-- Without using record syntax
data PWithThing a = PWithThing String a

name :: PWithThing a -> String
name (PWithThing name _) = name

thing :: PWithThing a -> a
thing (PWithThing _ thing) = thing

-- example usage
monkey :: PWithThing Integer
monkey = PWithThing "Monkey" 12

sloth :: PWithThing [Integer]
sloth = PWithThing "Sloth" [1,2,3,4]

--
-- Takes type variable
-- Using record syntax
data PWithThingRec a = PWithThingRec { rName :: String
                                      ,rThing :: a 
                                     }

-- example usage
tiger :: PWithThingRec String
tiger = PWithThingRec { rName = "tiger", rThing = "A string" }
   
-- Just a tuple where both elements are type variables
-- Has a function to access the inner tuple
data PTuple a b = PTuple (a, b)

getPTuple :: PTuple a b -> (a,b)
getPTuple (PTuple (x,y)) = (x,y)

-- example usage
somePair :: PTuple Integer Double
somePair = PTuple (12, 2.33)
   
-- Doing the same with record syntax
data PTupleRec a b = PTupleRec { getPTupleRec :: (a,b) }

-- example usage
whatAPair :: PTupleRec Integer [Integer]
whatAPair = PTupleRec (2,[1])

-- possible, but previous is more readable
anotherPair :: PTupleRec String [[Integer]]
anotherPair = PTupleRec { getPTupleRec = ("Hello", [[2,3],[2],[],[1,2,3,4,5]]) }

-- Creating a new type
newtype NewPTupleRec a b = NewPTupleRec { getNewPTupleRec :: (a,b) }

-- example usage
newPair :: NewPTupleRec Integer [String]
newPair = NewPTupleRec (2,["Hello", "there"])

-- possible, but previous is more readable
anotherNew :: NewPTupleRec String Integer
anotherNew = NewPTupleRec { getNewPTupleRec = ("Hello", 5) }

-- Functions

newtype SomeFunction a = SomeFunction (a -> (a,String))

getSomeFunction :: SomeFunction a -> (a -> (a,String))
getSomeFunction (SomeFunction f) = f

doubler :: (Num a) => SomeFunction a
doubler = SomeFunction (\x -> (x*2, "Doubled"))

addFiver :: (Num a) => SomeFunction a
addFiver = SomeFunction (\x -> (x+5, "Added Five"))

instance Monad SomeFunction where
    return x= SomeFunction (\x -> (x, "Returned"))
    SomeFunction sf >>= f = let (res, log) = f $ fst (getSomeFunction sf) in (\x -> (res, log ++ (snd (getSomeFunction sf))))

