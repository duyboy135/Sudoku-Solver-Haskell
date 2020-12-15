module SudokuSolver
where

import Sudoku

backtrack :: Sudoku -> Maybe Sudoku
backtrack sudoku =
    helper (getUnfilledCell sudoku) 
      where
        helper [] = Just sudoku 
        helper (cell:_) = iterChoices $ getPossibleOptions sudoku cell
          where
            iterChoices [] = Nothing
            iterChoices (x:xs) = case backtrack $ fillSudoku sudoku cell x of
                Nothing -> iterChoices xs
                Just foundSudoku -> Just foundSudoku
                