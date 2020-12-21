import Sudoku
import SudokuSolver

import System.Environment
import Data.Time.Clock    
import Text.Printf

main :: IO()
main = do
  t0 <- getCurrentTime 
  args <- getArgs
  let maxDepth = if null args then 0 else read (head args)
  sudoku <- readSudoku
  putStrLn "======================Input Sudoku======================"
  putStr $ showSudoku sudoku
  putStrLn "======================Output Sudoku======================"
  case parSearch sudoku maxDepth of 
    Just ans -> do
      putStr $ showSudoku ans
      if validateSudoku ans then
        putStrLn "====== Correct"
      else 
        putStrLn "====== Wrong Answer"
    Nothing  -> putStrLn "No Solution"
  t1 <- getCurrentTime 
  printf "Elapsed time: %.4fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
