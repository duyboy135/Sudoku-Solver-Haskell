import Sudoku
import SudokuSolver

main :: IO()
main = do
  sudoku <- readSudoku
  putStrLn "======================Input Sudoku======================"
  putStr $ showSudoku sudoku
  putStrLn "======================Output Sudoku======================"
  case backtrack sudoku of 
    Just ans -> do
      putStr $ showSudoku ans
      if validateSudoku ans then
        putStrLn "====== Correct"
      else 
        putStrLn "====== Wrong Answer"
    Nothing  -> putStrLn "No Solution"