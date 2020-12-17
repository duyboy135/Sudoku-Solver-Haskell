module SudokuSolver
  ( search
  , parSearch
  )
where

import Sudoku

import Control.Monad.Par
import Control.DeepSeq

generateNextStep :: Sudoku -> [Sudoku]
generateNextStep sudoku = 
  helper (getUnfilledCells sudoku) 
    where
      helper [] = []
      helper cells = 
        let cells = getUnfilledCells sudoku
            cellsWithOptions = (\cell -> 
                                (length $ getPossibleCellOptions sudoku cell, cell))
                                <$> cells
            toFillCell = snd $ minimum cellsWithOptions
        in
            fillSudoku sudoku toFillCell <$> getPossibleCellOptions sudoku toFillCell

search :: Sudoku -> Maybe Sudoku
search sudoku =
  helper (getUnfilledCells sudoku) 
  where
    helper [] = Just sudoku 
    helper _ = tryChoices $ generateNextStep sudoku
      where
        tryChoices [] = Nothing
        tryChoices (x:xs) = case search x of
            Nothing -> tryChoices xs
            Just foundSudoku -> Just foundSudoku


maxDepth :: Int 
maxDepth = 4

findResult :: [Maybe Sudoku] -> Maybe Sudoku
findResult [] = Nothing 
findResult (Just x : xs)  = Just x
findResult (Nothing : xs) = findResult xs

parSearch :: Sudoku -> Maybe Sudoku
parSearch sudoku =
  runPar $ generatePar 0 sudoku 
    where
      generatePar depth sudoku | depth >= maxDepth
        = return $ search sudoku
      generatePar depth sudoku =
        if null (getUnfilledCells sudoku)
          then return $ Just sudoku
        else
          do
            solnss <- parMapM (generatePar (depth + 1)) (generateNextStep sudoku)
            return $ findResult solnss
