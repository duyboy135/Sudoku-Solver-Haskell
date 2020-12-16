module Sudoku
  ( Sudoku
  , Cell (..)
  , readSudoku
  , showSudoku
  , fillSudoku
  , getUnfilledCells
  , getPossibleCellOptions
  , validateSudoku
  )
where

import Control.Monad        (replicateM)
import Data.List            (sort)
import Data.List.Split      (splitOn)
import Data.List.Unique     (sortUniq)

data Cell = Cell {
      row :: Int
    , col :: Int
  } deriving (Show, Eq, Ord)

type Sudoku = [Int]

readSudoku :: IO Sudoku
readSudoku = do
    lines <- replicateM 9 getLine
    let sudoku = concat $ fmap read .splitOn " " <$> lines
    return sudoku

showSudoku :: Sudoku -> String
showSudoku [] = ""
showSudoku s@(h:t) 
    | len `mod` 9 == 1 = show h <> "\n" <> rem
    | otherwise = show h <> " " <> rem
    where
        len = length s
        rem = showSudoku t

cellToInt :: Cell -> Int
cellToInt cell = (row cell - 1)*9 + (col cell - 1) 

intToCell :: Int -> Cell
intToCell x = Cell 
    {   row = x `div` 9 + 1
      , col = x `mod` 9 + 1
    }

fillSudoku :: Sudoku -> Cell -> Int -> Sudoku
fillSudoku sudoku cell num = 
    let pos   = cellToInt cell
        begin = take pos sudoku
        end   = drop (pos + 1) sudoku
    in begin <> [num] <> end

getSudokuCellsWithFunc :: Sudoku -> (Cell -> Bool) -> [Int]
getSudokuCellsWithFunc sudoku func = 
    helper sudoku 0
      where
        helper l pos  
            | pos == 81 = []
            | func (intToCell pos) =  head l : helper (tail l) (pos + 1)
            | otherwise = helper (tail l) (pos + 1)

getRow :: Sudoku -> Int -> [Int]
getRow sudoku r =
    getSudokuCellsWithFunc sudoku (\cell -> row cell == r)

getCol :: Sudoku -> Int -> [Int]
getCol sudoku c =
    getSudokuCellsWithFunc sudoku (\cell -> col cell == c)
                          
getSubSquare :: Sudoku -> Cell -> [Int]
getSubSquare sudoku corner =
    getSudokuCellsWithFunc sudoku 
                          (\cell -> (row cell >= row corner)
                                    && (row cell < row corner + 3)
                                    && (col cell >= col corner)
                                    && (col cell < col corner + 3)
                          )

getUnfilledCells :: Sudoku -> [Cell]
getUnfilledCells sudoku =
    helper sudoku 0
      where
        helper l pos  
            | pos == 81 = []
            | head l == 0 =  intToCell pos : helper (tail l) (pos + 1)
            | otherwise = helper (tail l) (pos + 1)

getPossibleCellOptions :: Sudoku -> Cell -> [Int]
getPossibleCellOptions sudoku cell =
    let rowCells    = getRow sudoku (row cell)
        colCells    = getCol sudoku (col cell)
        squareCells = getSubSquare sudoku 
                        Cell {row = (row cell - 1) `div` 3 * 3 + 1, 
                              col = (col cell - 1) `div` 3 * 3 + 1}
        forbidden = sortUniq $ rowCells <> colCells <> squareCells
    in
        filter (`notElem` forbidden) [1..9]

containAllDigits :: [Int] -> Bool 
containAllDigits digits =
    sort digits == [1..9]

validateSudoku :: Sudoku -> Bool 
validateSudoku sudoku = 
    let rows = [1..9]
        cols = [1..9]
        subSquares = [Cell{row = 3*r + 1, col = 3*c + 1} | r <- [0..2], c <- [0..2]]
    in
        and (containAllDigits . getRow sudoku <$> rows)
        && and (containAllDigits . getCol sudoku <$> cols)
        && and (containAllDigits . getSubSquare sudoku <$> subSquares)