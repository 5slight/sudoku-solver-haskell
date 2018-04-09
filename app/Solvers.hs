module Solvers where

import Types
import Avaliability

import Util as U

notSolvedFold :: [Int] -> [Int] -> Int -> [Int]
notSolvedFold list res key = if list !! key > 0 then res else key : res

notSolved :: [Int] -> [Int]
notSolved list = foldl notSolvedFold' [] [(len - 1), (len - 2)..0] where
  len = length list
  notSolvedFold' = notSolvedFold list

setListVal :: [Int] -> Int -> Int -> [Int]
setListVal list key val = a ++ (val:b) where
  (a, (_:b)) = splitAt key list

data DirectSolveState = DirectSolveState
  {
    prob :: [Int],
    solves :: [Int],
    failed :: Bool
  }

directSolveFold :: DirectSolveState -> Int -> DirectSolveState
directSolveFold (DirectSolveState list slvs _) index =
  DirectSolveState newList newSlvs hasFailed where
  solved = avLen == 1
  newList = if solved then setListVal list index avf else list
  newSlvs = if solved then index : slvs else slvs
  hasFailed = avLen < 1
  av = getAvaliable index list
  (avf:_) = av
  avLen = length av

directNewOp :: Int -> Int -> Bool -> SolveOp
directNewOp _ _ True   = Failed
directNewOp 0 _ False  = Solved
directNewOp _ 0 _      = GuessSolve
directNewOp _ _ _      = DirectSolve

newGuessWithSolves :: Guess -> [Int] -> Guess
newGuessWithSolves g slvs =
  Guess (guessIndex g) (guessAttempts g) (slvs ++ (guessSolves g))

newGuessesWithSolves :: [Guess] -> [Int] -> [Guess]
newGuessesWithSolves gs slvs =
  newGuessWithSolves (head gs) slvs : U.sTail gs

directSolve :: SolvingState -> SolvingState
directSolve (SolvingState list _ g) = SolvingState newList newOp newG where
  toSolve = notSolved list
  newOp = directNewOp (length toSolve) sc hf
  startState = (DirectSolveState list [] False)
  isGuessing = length g > 0
  newG = if isGuessing then newGuessesWithSolves g slvs else g
  (DirectSolveState newList slvs hf) = foldl directSolveFold startState toSolve
  sc = length slvs

guessesHead :: Int -> [Guess] -> Guess
guessesHead _ (gf:_) = gf
guessesHead index [] = Guess index (0 - 1) []

guessSolve :: SolvingState -> SolvingState
guessSolve (SolvingState list _ g) =
  SolvingState newList DirectSolve newGuessing
  where
    (tg:_) = notSolved list
    guess = guessesHead tg g
    guessMatch = guessIndex guess == tg
    av = getAvaliable tg list
    avIdx = if guessMatch then guessIndex guess + 1 else 0
    newList = setListVal list tg $ av !! avIdx
    newGuess = Guess tg avIdx []
    newGuessing = (if guessMatch then newGuess : U.sTail g else newGuess : g)


undoGuess :: SolvingState -> SolvingState
undoGuess ss = ss
