module MiniMax where

import System.IO
import Data.List
import Data.Array
import Data.Foldable ( asum )

import Logic


availableCells :: Cell -> Board -> [Cell]
availableCells cell = filter ((==) cell) . elems


-- if space is available, move there, send updated game to minimax and update the 'score' variable
-- isAvailable cell =
-- if the score from minimax is greater than the current best score, set that as the new best score
-- return the 'bestMove' with the best score
bestMove :: Game -> (Int,Int)
bestMove game = do
  -- 
  if (gameBoard game ! (i,j) == Nothing)
    then do
      let game1 = game { gamePlayer = PlayerO
                    , gameBoard = board // [(bestMove, Just PlayerX)] }
      -- pass updated game to minimax, get its score and store it as a variable
      let score = game1
      -- score = minimax game1 1 True
      -- if the score recieved from minimax is greater than the current score, that score is now the best score
      if (score > bestScore)
        then do
          let bestScore' = score
          bestScore'
          -- (i,j)
      else 
        score
  else 
    return 
  where i = 0
        j = 0
        bestScore = -1000
        bestMove = (0,0)
        board = gameBoard game

-- keep track of best move score
-- for each move, if available
  -- if cell available, player turn there then call minimax on the board
  -- if the score of this move is greater than the current best score, 
  --   then current move's score is now the new best score and the best move is this paticular move
  -- be

minimaxScores :: Game -> Int
minimaxScores game
  | x == GameOver (Just PlayerX) = 1
  | x == GameOver (Just PlayerO) = -1
  | otherwise = 0
    where 
      x = gameState game



-- minimax :: Game -> Int -> Bool -> Int
-- minimax game depth isMaximixing = do
--   result <- checkGameOver game
--   -- if game is over from move, return score of game
--   if (gameState result /= Running) 
--     then 
--       minimaxScores (result)
--   -- if its the maximixing players turn...
--   else if (isMaximixing)
--     then do
--       if (gameBoard game ! (i,j) == Nothing)
--         then do
--           let game1 = game { gamePlayer = PlayerO
--                           , gameBoard = board // [(bestMove, Just PlayerX)] }
--           score <- minimax game1 (depth + 1) False
--           if (score > bestScore)
--             then do
--               let bestScore = score
--               (i,j)
--           else 
--              score
--       else
--        bestScore
--   else
--    bestScore
--   where 
--     i = [0..2]
--     j = [0..2]
--     bestScore = -1000
--     bestMove = (0,0)
--     board = gameBoard game
  

-- bestMove choice (minMax, minMax') (p0, p1) (s0, s1) moves = minMax $ check <$> moves
--     where check move
--             | checkWin (p0 + move) = (s0, choice')
--             | null moves' = (0, choice')
--             | otherwise = bestMove choice' (minMax', minMax) (p1, p0 + move) (s1, s0) moves'
--                 where moves' = delete move moves
--                       choice' = if choice < 0 then move else choice



testForMappingCells game = 
  map full row
    where
      row = [[gameBoard game ! (j,i) | i <- [0..2]] | j <- [0..2]]

qweqwe game =
  putStrLn (show yuh)
    where 
      yuh = [[gameBoard game ! (j,i) | i <- [0..2]] | j <- [0..2]]

initialGameBoard =
  gameBoard initialGame