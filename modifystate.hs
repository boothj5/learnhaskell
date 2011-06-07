import Control.Monad.State

data Game = Game { numPlayers      :: Int
                 , numCardsEach    :: Int
                 , lastMove        :: String
                 }
    deriving (Show)
                 
emptyGame :: Game
emptyGame = Game { numPlayers = 0, numCardsEach = 0, lastMove = "" }

--main = do
--    startGame
--    putStrLn "Done main"
    
getPlayers = do
    cps <- gets numPlayers
    lift $ putStrLn ("Enter num players: " ++ show cps)
    ps <- lift $ fmap read getLine
    modify $ \st -> st { numPlayers = ps }

getCards = do
    lift $ putStrLn "Enter num cards: "
    cs <- lift $ fmap read getLine
    modify $ \st -> st { numCardsEach = cs }

startGame = do
    modify $ \st -> st { lastMove = "Start" }
    getPlayers
    getCards
    players <- gets numPlayers
    cards <- gets numCardsEach
    move <- gets lastMove
    lift $ putStrLn (show players)
    lift $ putStrLn (show cards)
    lift $ putStrLn (show move)
    
main = do
    evalStateT startGame emptyGame
    