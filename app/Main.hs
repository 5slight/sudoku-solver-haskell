module Main where

import Types
import qualified Output as O
import qualified Solvers as S

import qualified Data.Map.Strict as Map

problemt :: [Int]
problemt = [
  5, 0, 8, 0, 0, 0, 0, 0, 0,
  0, 2, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 3, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 4, 0, 0, 0, 0, 0,
  6, 0, 0, 0, 5, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 6, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 7, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 8, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 9
  ]

problem1 :: [Int]
problem1 = [
  5, 0, 8, 0, 3, 0, 0, 0, 9,
  0, 0, 0, 0, 9, 4, 1, 5, 0,
  9, 1, 0, 0, 0, 0, 0, 0, 8,
  0, 0, 0, 9, 6, 2, 3, 4, 0,
  2, 0, 0, 5, 0, 3, 0, 0, 6,
  0, 6, 5, 7, 4, 8, 0, 0, 0,
  6, 0, 0, 0, 0, 0, 0, 9, 7,
  0, 8, 7, 6, 2, 0, 0, 0, 0,
  1, 0, 0, 0, 5, 0, 8, 0, 4]

problem2 :: [Int]
problem2 = [
  2, 0, 4, 1, 6, 3, 7, 0, 0,
  5, 0, 0, 0, 7, 4, 0, 0, 8,
  0, 7, 0, 0, 0, 0, 0, 0, 4,
  1, 2, 0, 7, 0, 6, 0, 0, 0,
  0, 5, 0, 0, 8, 0, 0, 2, 0,
  0, 0, 0, 4, 0, 1, 0, 8, 7,
  6, 0, 0, 0, 0, 0, 0, 9, 0,
  8, 0, 0, 6, 3, 0, 0, 0, 1,
  0, 0, 1, 9, 4, 2, 8, 0, 5]

problem :: [Int]
problem = [
  0, 0, 4, 9, 0, 0, 0, 0, 6,
  3, 9, 0, 0, 0, 1, 0, 0, 8,
  0, 0, 0, 0, 7, 4, 0, 0, 9,
  0, 0, 0, 4, 3, 0, 0, 5, 0,
  4, 0, 1, 0, 5, 0, 2, 0, 3,
  0, 2, 0, 0, 1, 9, 0, 0, 0,
  7, 0, 0, 5, 9, 0, 0, 0, 0,
  9, 0, 0, 1, 0, 0, 0, 3, 5,
  1, 0, 0, 0, 0, 6, 9, 0, 0]

solveSelect :: SolveOp -> SolvingState -> SolvingState
solveSelect DirectSolve state = solve $ S.directSolve state
solveSelect GuessSolve state = solve $ S.guessSolve state
solveSelect UndoGuess state = state
solveSelect _ state = state

solve :: SolvingState -> SolvingState
solve state = solveSelect (solveOp state) state

initial :: [Int] -> SolvingState
initial list = SolvingState list DirectSolve []

main :: IO ()
main = O.pprintList list where
  (SolvingState list _ _) = solve $ initial problem
