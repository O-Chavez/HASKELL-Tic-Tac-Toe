import System.IO
import Data.List
import Data.Array
import Data.Foldable ( asum )

import Logic

-- moves can be determined by using the inRange predicate on an array from ((0,0), (2,2)) and use the index of each coordinate as a placement variable... (e.g top left move would be (0,0) with an index of 0 and the bottom right would be (2,2) with an index of 8. displays moves as Ints 1-9, so move = n - 1 == coordinate index)

readInt :: IO Int
readInt = do
  m <- getLine
  return (read m :: Int)

moveNum :: Int -> (Int, Int)
moveNum n = 
  indexRange !! (n - 1)
    where
      indexRange = range ((0,0), (2,2))

isMoveCorrect :: (Int,Int) -> Bool
isMoveCorrect n = inRange ((0,0), (2,2)) n

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game coords =
-- check if move is correct
  if (isMoveCorrect coords)
    -- update inputted move to board
    then
      -- switch players and return updated game
      case gamePlayer game of
        PlayerX -> game { gamePlayer = PlayerO
                        , gameBoard = board // [(coords, Just player)] }
        playerO -> game { gamePlayer = PlayerX
                        , gameBoard = board // [(coords, Just player)] }
    else game
  where board = gameBoard game
        player = gamePlayer game


-- printGame :: Game -> Game
printGame game =
  print (cellsOfBoard game)


-- gameDisplay :: Game -> String
-- gameDisplay game = 
--   case gameState game of
--     Running ->
--       display
--     GameOver _ -> display
  

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

indexRange = range ((0,0), (2,2))

cellContents :: Cell -> String
cellContents cell =
  case cell of
    Just PlayerO -> "O"
    Just PlayerX -> "X"
    Nothing -> " "
    


-- create a function that takes in a game and cell coordinates and gives you the cell...
getCell :: Game -> (Int,Int) -> Cell
getCell game coords =
  cellsOfBoard game!coords


boardDisplay :: Game -> IO ()
boardDisplay g =
  putStr (cellLine1 ++ rowLine ++ cellLine2 ++ rowLine ++ cellLine3)
    where
    rowLine = ("  " ++ ( intercalate "+" $ take 3 $ repeat "---") ++ "   \n")
    cellLine1 = ("   " ++  cellContents (getCell g (0,0)) ++ " | " ++  cellContents (getCell g (0,1)) ++ " | " ++  cellContents (getCell g (0,2)) ++ " \n")
    cellLine2 = ("   " ++  cellContents (getCell g (1,0)) ++ " | " ++  cellContents (getCell g (1,1)) ++ " | " ++  cellContents (getCell g (1,2)) ++ " \n")
    cellLine3 = ("   " ++  cellContents (getCell g (2,0)) ++ " | " ++  cellContents (getCell g (2,1)) ++ " | " ++  cellContents (getCell g (2,2)) ++ " \n")
  



gameLoop :: Game -> IO ()
gameLoop g = do
  -- check if game is over
  

  -- display initial board
  boardDisplay g
  -- get and handle player move
  putStrLn ("Please enter a move 1-9...")
  n <- readInt
  -- if move is valid...
  if (n <= 9 && isMoveCorrect (moveNum n) && gameState g == Running)
    then 
      -- update game

      let updatedGame = playerTurn g (moveNum n) in do
      putStrLn (show updatedGame)
      -- boardDisplay updatedGame
      gameLoop updatedGame
      
  else if (gameState g == GameOver (Nothing))
    then
      putStrLn ("Game Over! Its a Tie!!!")
  else if (gameState g == GameOver (Just PlayerX))
    then
      putStrLn ("Game Over! X Wins!!!")
  else if (gameState g == GameOver (Just PlayerX))
    then
      putStrLn ("Game Over! O Wins!!!")
  else do
    putStrLn ("--- MOVE -"  ++ show n ++  "- NOT VALID! Please enter a number 1-9 ---")
    gameLoop g
  
  
  -- recall gameLoop with updated game
  -- gameLoop


main :: IO ()
main = do
  -- Intro
  putStrLn ("----------- WELCOME TO TIC-TAC-TOE -----------")
  putStrLn ("Wanna play ticky boy? y/n")
  input <- getLine
  if input == "y" then
    gameLoop initialGame
  else if input == "n" then
    putStrLn ("ah dang alright. gg")
  else do 
    putStrLn "Idk man,"
      

display1 = do
  putStrLn ("Game over!")

