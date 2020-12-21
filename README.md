# Haskell Sudoku Solver
## Introduction
Sudoku solver written in Haskell using classical backtrack algorithm.
Project can be built and run with stack environment.
## Build
```
stack build
```
## Run
```
stack exec -- sudoku-solver-haskell-exe <maxDepth> <InputData/input<x>.txt
```
### Options:
maxDepth: maximum level in the recursive tree where we process each branch subtree parallelly. If maxDepth=0, then it's a normal recurisve algorithm without parallelization. Default option for <maxDepth> is 0 if we don't have any command line arguments. 
### Sample Sudoku
Stored in InputData. Examples:
1. InputData/input5.txt: Empty sudoku
2. InputData/input7.txt: Hardest sudoku in the world
