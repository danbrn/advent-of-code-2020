-- | Advent of Code 2020 - Day 10

module Day10
    ( day10
    , day10'
    )
where

import           AoC
import           Data.List                      ( group
                                                , sort
                                                )

day10 :: [String] -> Int
day10 input = length (filter (== 1) dvs) * length (filter (== 3) dvs)
  where
    vs  = parse input
    dvs = diffs vs

day10' :: [String] -> Int
day10' input = product $ map ways $ group dvs
  where
    vs  = parse input
    dvs = diffs vs
    ways xs@(x : _)
        | x == 3 = 1
        | x == 1 = case length xs of
            1 -> 1
            2 -> 2
            3 -> 4
            4 -> 7
            _ -> error "calculate more ways"

parse = (\xs -> 0 : xs ++ [maximum xs + 3]) . sort . map read

diffs vs = zipWith (-) (tail vs) vs
