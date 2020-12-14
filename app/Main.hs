import Sudoku
import SudokuSolver
import Data.Maybe       (fromJust)

main :: IO()
main = do
  sudoku <- readSudoku
  putStrLn "======================Input Sudoku======================"
  putStr $ showSudoku sudoku
  putStrLn "======================Output Sudoku======================"
  case backtrack sudoku of 
    Just ans -> putStr $ showSudoku ans
    Nothing  -> putStrLn "No Solution"
