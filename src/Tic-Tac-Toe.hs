import System.IO
import Data.List
import Data.Array
import Data.Maybe
import Text.Read
import Logic
import Visuals
-- import MiniMax

main :: IO ()
main = do
  -- Intro
  putStrLn ("\n----------- WELCOME TO TIC-TAC-TOE -----------\n")
  putStrLn ("Would you like to play a game? y/n")
  input <- getLine
  if input == "y" then do
    moveGuide
    gameLoop initialGame
  else
      putStrLn ("Okay, Goodbye!")

-- moves can be determined by using the inRange predicate on an array from ((0,0), (2,2)) and use the index of each coordinate as a placement variable... (e.g top left move would be (0,0) with an index of 0 and the bottom right would be (2,2) with an index of 8. displays moves as Ints 1-9, so move = n - 1 == coordinate index)
gameLoop :: Game -> IO ()
gameLoop game = do
  -- UPDATE GAMESTATE AND CHECK TO SEE IF GAME IS OVER
  let gameStep2 = checkGameOver (game)
  -- if game is still running...
  if (gameState gameStep2 == Running)
    then do
      -- display board
      moveGuide
      boardDisplay gameStep2
      -- get and handle player move
      putStrLn (show (gamePlayer gameStep2) ++ " : Please enter a move 1-9...")
      n <- readInt
      -- if move is valid... 
      if (n <= 9 && isMoveCorrect (moveNum n)(gameStep2) && gameState gameStep2 == Running)
        then 
          -- update game
          let updatedGame = playerTurn (gameStep2) (moveNum n) in do
          gameLoop updatedGame
      else do
        putStrLn ("\n--- MOVE -"  ++ show n ++  "- NOT VALID! Please enter a number 1-9 ---")
        gameLoop gameStep2
  -- if player X wins...
  else if (gameState (gameStep2) == GameOver (Just PlayerX))
    then do
      boardDisplay gameStep2
      putStrLn ("\n----- Game Over! X Wins!!! -----")
      main
  -- if player O wins...
  else if (gameState (gameStep2) == GameOver (Just PlayerO))
    then do
      boardDisplay gameStep2
      putStrLn ("\n----- Game Over! O Wins!!! -----")
      main
   -- if its a tie
  else do
      putStrLn ("\nGame Over! Its a Tie!!!")
      main

-- readInt :: IO Int
readInt = do
  m <- getLine
  case readMaybe m :: Maybe Int of
    Just m -> return (m :: Int)
    Nothing -> return 999

moveNum :: Int -> (Int, Int)
moveNum n = 
  indexRange !! (n - 1)
    where
      indexRange = range ((0,0), (2,2))

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game coords =
-- check if move is correct
  if (isMoveCorrect coords game)
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