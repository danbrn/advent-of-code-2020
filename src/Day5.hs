-- | Advent of Code 2020 - Day 5

module Day5
  ( day5
  , day5'
  )
where

import           AoC

import           Numeric
import           Data.Maybe                     ( fromJust
                                                , listToMaybe
                                                )
import           Data.List                      ( sort )

readBinary chars =
  fromJust . fmap fst . listToMaybe . readInt 2 (`elem` chars) letterValue
 where
  letterValue l | l == head chars = 0
                | otherwise       = 1

readRow = readBinary "FB"
readCol = readBinary "LR"

seat s = r * 8 + c
 where
  (r', c') = span (`elem` "FB") s
  r        = readRow r'
  c        = readCol c'

day5 :: [String] -> Int
day5 = maximum . map seat

day5' :: [String] -> Int
day5' xs = succ $ fst $ head $ filter ((== 2) . uncurry (flip (-))) ss'
 where
  ss  = sort $ map seat xs
  ss' = zip ss (tail ss)
