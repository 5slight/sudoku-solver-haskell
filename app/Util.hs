module Util where

import GHC.Float

wholeSqrt :: Int -> Int
wholeSqrt num = round $ float2Double $ sqrt $ fromIntegral num

getSize :: [Int] -> Int
getSize list = wholeSqrt $ length list

sTail :: [t] -> [t]
sTail l
  | null l = []
  | otherwise = tail l
