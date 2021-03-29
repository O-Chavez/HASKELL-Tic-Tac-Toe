module Logic where

import System.IO
import Data.List
import Data.Array
import Data.Foldable ( asum )

data Player = PlayerX | PlayerO deriving (Eq, Show)
type Cell = Maybe Player
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

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


checkGameOver :: Game -> Game
checkGameOver game
  | Just p <- winner board =
    game { gameState = GameOver $ Just p }
  | countCells Nothing board == 0 =
    game { gameState = GameOver Nothing }
  | otherwise = game
  where board = gameBoard game

