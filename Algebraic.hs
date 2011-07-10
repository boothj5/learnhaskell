data Player = HumanPlayer { hpcards :: [Int] }
            | ComputerPlayer { cpcards :: [Int] }

getCard :: Player -> Int
getCard (HumanPlayer cs)    = minimum cs
getCard (ComputerPlayer cs) = maximum cs

human = HumanPlayer { hpcards = [1,2,3,4,5] }
computer = ComputerPlayer { cpcards = [1,2,3,4,5] }

playerList :: [Player]
playerList = [ human
             , computer ]


