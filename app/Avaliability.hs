module Avaliability(getAvaliable, vCalc, hCalc, foldBoxRows) where

import GHC.Float

import qualified Util as U
import qualified Data.List.Split as S
import qualified Data.IntSet as IntSet

vCalc :: Int -> Int -> Int
vCalc index size = floor $ float2Double (fromIntegral index / fromIntegral size)

hCalc :: Int -> Int -> Int
hCalc = mod

filterZero :: [Int] -> [Int]
filterZero list = filter (>0) list

getRow :: Int -> Int -> [[Int]] -> [Int]
getRow index size rows = filterZero $ rows !! vCalc index size

colsFromRows :: Int -> [[Int]] -> [Int]
colsFromRows cindex (row:rest) = row !! cindex : colsFromRows cindex rest
colsFromRows _ [] = []

getColumn :: Int -> Int -> [[Int]] -> [Int]
getColumn index size rows = filterZero $ colsFromRows (hCalc index size) rows

foldBoxCols :: Int -> Int -> Int -> [Int] -> [Int] -> Int -> [Int]
foldBoxCols index size boxSize row res val =
  if match
  then if item > 0 then item : res else res
  else res
  where
    item = row !! val
    match = bindexTest == box
    bindexTest = vCalc val boxSize
    col = hCalc index size
    box = vCalc col boxSize

foldBoxRows :: Int -> Int -> Int -> [[Int]] -> [Int] -> Int -> [Int]
foldBoxRows index size boxSize rows res val =
  if match
  then foldl foldBoxCols' [] [0,1..(size - 1)] ++ res
  else res
  where
    foldBoxCols' = foldBoxCols index size boxSize (rows !! val)
    match = bindexTest == box
    bindexTest = vCalc val boxSize
    row = vCalc index size
    box = vCalc row boxSize

getBox :: Int -> Int -> [[Int]] -> [Int]
getBox index size rows = foldl foldBoxRows' [] [0,1..(size - 1)] where
  foldBoxRows' = foldBoxRows index size boxSize rows
  boxSize = U.wholeSqrt size

foldAvaliable :: [IntSet.IntSet] -> [Int] -> Int -> [Int]
foldAvaliable groups res item =
  if any (\set -> IntSet.member item set) groups
  then res
  else item : res

getAvaliable :: Int -> [Int] -> [Int]
getAvaliable index list = foldl (foldAvaliable') [] range
  where
    size = U.getSize list
    range = [1,2..size]
    rows = S.chunksOf size list
    row = IntSet.fromList $ getRow index size rows
    col = IntSet.fromList $ getColumn index size rows
    box = IntSet.fromList $ getBox index size rows
    foldAvaliable' = foldAvaliable [row, col, box]
