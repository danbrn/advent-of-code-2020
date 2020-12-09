-- | Advent of Code 2020 - Day 9

module Day9
    ( day9
    , day9'
    )
where

import           AoC
import           Data.List                      ( tails )
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , isJust
                                                )

day9 :: [String] -> Int
day9 =
    head
        . filter (/= 0)
        . map isSum
        . filter ((> 25) . length)
        . tails
        . map read
  where
    isSum ns = findSum number (take 25 ns)
      where
        number = ns !! 25
        findSum _ []       = number
        findSum t (x : xs) = if (t - x) `elem` ns then 0 else findSum t xs

day9' :: [String] -> Int
day9' input =
    (\xs -> maximum xs + minimum xs)
        . head
        . filter ((> 2) . length)
        . catMaybes
        . map (findSum n)
        $ tails ns
  where
    n  = day9 input
    ns = map read input

findSum = findSum' []
  where
    findSum' _ _ [] = Nothing
    findSum' acc n (x : xs) | sum acc + x == n = Just (x : acc)
                            | otherwise        = findSum' (x : acc) n xs
