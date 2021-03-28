import System.IO
import Data.List
import Data.Array
import Data.Foldable ( asum )

data Player = PlayerX | PlayerO deriving (Eq, Show)
type Cell = Maybe Player
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

-- data GameBoard = GameBoard { boardSize :: Array, }

xCell :: String
xCell = " "
oCell :: String
oCell = " "

type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board
                ,  gamePlayer :: Player
                ,  gameState :: State
                } deriving (Eq, Show)


initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   , gamePlayer = PlayerX
                   , gameState = Running
                   }
    where indexRange = ((0,0), (2,2))



switchPlayers game =
  case gamePlayer game of
    PlayerX -> game { gamePlayer = PlayerO }
    playerO -> game { gamePlayer = PlayerX }

full :: [Cell] -> Maybe Player
full (cell@(Just player):cells) | all (== cell) cells = Just player
full _                                                = Nothing

winner :: Board -> Maybe Player
winner board = asum $ map full $ rows ++ rows ++ columns ++ diagnals
  where rows = [[board ! (i,j) | i <- [0..2]] | j <- [0..2]]
        columns = [[board ! (i,j) | i <- [0..2]] | j <- [0..2]]
        diagnals = [[board ! (i,j) | i <- [0..2], let j = 2-j ]]

countCells :: Cell -> Board -> Int
countCells cell = length . filter ((==) cell) . elems

checkGameOver game
  | Just p <- winner board =
    game { gameState = GameOver $ Just p }
  | countCells Nothing board == 0 =
    game { gameState = GameOver Nothing }
  | otherwise = game
  where board = gameBoard game


-- moves can be determined by using the inRange predicate on an array from ((0,0), (2,2)) and use the index of each coordinate as a placement variable... (e.g top left move would be (0,0) with an index of 0 and the bottom right would be (2,2) with an index of 8. displays moves as Ints 1-9, so move = n - 1 == coordinate index)
moveNum :: Int -> (Int, Int)
moveNum n = 
  indexRange !! (n - 1)
    where
      indexRange = range ((0,0), (2,2))

isMoveCorrect = inRange ((0,0), (2,2))

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game coords =
-- check if move is correct
  if (isMoveCorrect coords)
    -- update inputted move to board
    then game { gameBoard = board // [(coords, Just player)]}
    else game
  where board = gameBoard game
        player = gamePlayer game
-- update inputted move to board

testTurn :: Game -> Game
testTurn game = do
  putStrLn ("enter something")
  n <- getLine
  playerTurn game n
       

-- playerTurn game moveNum
--   | isMoveCorrect moveNum && board ! moveNum == Nothing =
--     checkGameOver
--     $ switchPlayers
--     $ game { gameBoard = board // [(moveNum, Just player)] }
--     -- $ printGame game - this line will cause issues
--   | otherwise = game
--   where board = gameBoard game
--         player = gamePlayer game


-- printGame :: Game -> Game
printGame game =
  print (cellsOfBoard game)


-- take in move of game
-- takeMove :: Int -> Game -> Game
-- takeMove n game = 
--   putStrLn "Please enter a move (1-9)"
--   do playerTurn n <- n
--   case gameState of
--     Running -> playerTurn game $ n
--     GameOver _ -> initialGame
-- takeMove _ game = game

-- gameAsRunning board =
--   putStrLn (" " ++ Cell ++ " | " ++ Cell ++ " | " ++ Cell ++ " ")
--   putStrLn "---+---+---"
--   putStrLn (" " ++ Cell ++ " | " ++ Cell ++ " | " ++ Cell ++ " ")
--   putStrLn "---+---+---"
--   putStrLn (" " ++ Cell ++ " | " ++ Cell ++ " | " ++ Cell ++ " ")

-- gameDisplay :: Game -> String
gameDisplay game = 
  case gameState game of
    Running ->
      display
    GameOver _ -> display
  

display n = do
  putStrLn (" " ++ "0" ++ " | " ++ "0" ++ " | " ++ "0" ++ " ")
  putStrLn "---+---+---"
  putStrLn (" " ++ "0" ++ " | " ++ "0" ++ " | " ++ "0" ++ " ")
  putStrLn "---+---+---"
  putStrLn (" " ++ "0" ++ " | " ++ "0" ++ " | " ++ "0" ++ " ")
  putStrLn "Please enter a move"
  n <-  getLine
  putStrLn ("your move was " ++ n)


-- Use record syntax to get the gameBoard of the given game
-- cellsOfBoard :: Game -> Array
cellsOfBoard game =
  gameBoard game 

-- use  - initialGameCells!(0,0) - to get the contents of the first cell!)
initialGameCells = gameBoard initialGame


-- all game cells
cell1 = initialGameCells!(0,0)
cell2 = initialGameCells!(0,1)
cell3 = initialGameCells!(0,2)
cell4 = initialGameCells!(1,0)
cell5 = initialGameCells!(1,1)
cell6 = initialGameCells!(1,2)
cell7 = initialGameCells!(2,0)
cell8 = initialGameCells!(2,1)
cell9 = initialGameCells!(2,2)

indexRange = range ((0,0), (2,2))

getCell :: Cell -> String
getCell cell =
  case cell of
    Just PlayerO -> "O"
    Just PlayerX -> "X"
    _ -> " "
    

boardDisplay =
  putStr (cellLine1 ++ rowLine ++ cellLine2 ++ rowLine ++ cellLine3)
    where
    rowLine = ("  " ++ ( intercalate "+" $ take 3 $ repeat "---") ++ "   \n")
    cellLine1 = ("   " ++ getCell cell1 ++ " | " ++ getCell cell2 ++ " | " ++ getCell cell3 ++ " \n")
    cellLine2 = ("   " ++ getCell cell4 ++ " | " ++ getCell cell5 ++ " | " ++ getCell cell5 ++ " \n")
    cellLine3 = ("   " ++ getCell cell7 ++ " | " ++ getCell cell8 ++ " | " ++ getCell cell9 ++ " \n")


-- testDisplayOfBoardWithCells :: Game -> String
testDisplayOfBoardWithCells = do
  boardDisplay
  putStrLn ("whats your move chief?")
  n <- getLine
  -- playerTurn initialGame moveNum n
  -- putStrLn (show cellsOfBoard)
  putStrLn ("dang... - " ++ n)

lilTest = do
  putStrLn ("WELCOME TO TIC-TAC-TOE")
  putStrLn ("shit man, wanna play ticky boy? y/n")
  input <- getLine
  if input == "y" then
    testDisplayOfBoardWithCells
  else if input == "n" then
    putStrLn ("ah damn alright. gg")
  else do 
    putStrLn "Idk man,"
      
  
  
 



display1 = do
  putStrLn ("Game over!")

