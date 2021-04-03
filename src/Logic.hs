module Logic where

import System.IO
import Data.List
import Data.Array
import Data.Foldable ( asum )

data Player = PlayerX | PlayerO deriving (Eq, Show)
type Cell = Maybe Player
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

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

full :: [Cell] -> Maybe Player
full (cell@(Just player):cells) | all (== cell) cells = Just player
full _                                                = Nothing


-- cpuMove :: -> Game -> Game
-- cpuMove game= 
-- -- check if move is correct
--   if (isMoveCorrect coords game)
--     -- update inputted move to board
--     then
--       -- switch players and return updated game
--       case gamePlayer game of
--         PlayerX -> game { gamePlayer = PlayerO
--                         , gameBoard = board // [(coords, Just player)] }
--         playerO -> game { gamePlayer = PlayerX
--                         , gameBoard = board // [(coords, Just player)] }
--     else cpuMove
--   where board = gameBoard game
--         player = gamePlayer game


winningPos :: Board -> Maybe Player
winningPos board = 
  -- see if a single player has occupied all three spots in any winning postion... (diags, columns, rows...)
  asum $ map full $ columns ++ rows ++ diagnal1 ++ diagnal2
    where 
      columns =   [[board ! (j,i) | i <- [0..2]] | j <- [0..2]]
      rows =      [[board ! (i,j) | i <- [0..2]] | j <- [0..2]]
      diagnal1 =  [[board ! (i,i) | i <- [0..2]]]
      diagnal2 =  [[board ! (i,j) | i <- [0..2], let j = 2-i ]]

countCells :: Cell -> Board -> Int
countCells cell = length . filter ((==) cell) . elems

isMoveCorrect :: (Int,Int) -> Game -> Bool
isMoveCorrect n g = 
  inRange ((0,0), (2,2)) n && cellsOfBoard g!n == Nothing

-- Use record syntax to get the gameBoard of the given game
-- cellsOfBoard :: Game -> Array
cellsOfBoard game =
  gameBoard game 

checkGameOver :: Game -> Game
checkGameOver game
  -- look to see if a there is a player in a winning postion...
  | Just player <- winningPos board =
      game { gameState = GameOver $ Just player }
  -- look for tie...
  | countCells Nothing board == 0 =
      game { gameState = GameOver Nothing }
  | otherwise = game
  where board = gameBoard game