module SudokuSolver
  ( backtrack )
where

import Sudoku

backtrack :: Sudoku -> Maybe Sudoku
backtrack sudoku =
    helper (getUnfilledCells sudoku) 
      where
        helper [] = Just sudoku 
        helper (cell:_) = tryChoices $ getPossibleCellOptions sudoku cell
          where
            tryChoices [] = Nothing
            tryChoices (x:xs) = case backtrack $ fillSudoku sudoku cell x of
                Nothing -> tryChoices xs
                Just foundSudoku -> Just foundSudoku