{-# LANGUAGE ExistentialQuantification #-}

data HumanPlayer = HumanPlayer { hpcards :: [Int] }
data ComputerPlayer = ComputerPlayer { cpcards :: [Int] }

class Player a where
    getCard :: a -> Int

instance Player HumanPlayer where
    getCard = minimum . hpcards

instance Player ComputerPlayer where
    getCard = maximum . cpcards


data Playable = forall a . Player a => MakePlayable a

pack :: Player a => a -> Playable
pack = MakePlayable

human = HumanPlayer { hpcards = [1,2,3,4,5] }
computer = ComputerPlayer { cpcards = [1,2,3,4,5] }

playerList :: [Playable]
playerList = [ pack human
             , pack computer ]

f (MakePlayable a) = getCard a

