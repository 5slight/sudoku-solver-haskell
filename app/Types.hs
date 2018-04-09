module Types where

import qualified Data.Map.Strict as Map

data SolveOp = Solved | Failed | DirectSolve | GuessSolve | UndoGuess

data SolvingState = SolvingState
  {
    prob :: [Int],
    solveOp :: SolveOp,
    guesses :: [Guess]
  }

data Guess = Guess
  {
    guessIndex :: Int,
    guessAttempts :: Int,
    guessSolves :: [Int]
  }
