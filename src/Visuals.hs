module Visuals where

import System.IO
import Data.List
import Data.Array
import Data.Foldable ( asum )

import Logic

-- moveGuide :: IO ()
moveGuide = do
  putStrLn ("\n---MOVE GUIDE---")
  putStrLn ("   1 | 2 | 3   ")
  putStrLn "  ---+---+---"
  putStrLn ("   4 | 5 | 6   ")
  putStrLn "  ---+---+---"
  putStrLn ("   7 | 8 | 9   ")
  putStrLn ("----------------")

boardDisplay :: Game -> IO ()
boardDisplay g =
  putStr (cellLine1 ++ rowLine ++ cellLine2 ++ rowLine ++ cellLine3)
    where
    rowLine = ("  " ++ ( intercalate "+" $ take 3 $ repeat "---") ++ "   \n")
    cellLine1 = ("   " ++  cellContents (getCell g (0,0)) ++ " | " ++  cellContents (getCell g (0,1)) ++ " | " ++  cellContents (getCell g (0,2)) ++ " \n")
    cellLine2 = ("   " ++  cellContents (getCell g (1,0)) ++ " | " ++  cellContents (getCell g (1,1)) ++ " | " ++  cellContents (getCell g (1,2)) ++ " \n")
    cellLine3 = ("   " ++  cellContents (getCell g (2,0)) ++ " | " ++  cellContents (getCell g (2,1)) ++ " | " ++  cellContents (getCell g (2,2)) ++ " \n")

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