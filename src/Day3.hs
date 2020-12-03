-- | Advent of Code 2020 - Day 3

module Day3
    ( day3
    , day3'
    )
where

import           AoC

day3 :: [String] -> Int
day3 xs = countTrees (buildMap xs) 3 1

day3' :: [String] -> Int
day3' xs = product
    $ map (uncurry $ countTrees m) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    where m = buildMap xs

buildMap = map cycle

pos m x y = m !! y !! x

isTree m x y = pos m x y == '#'

slope m x y = zip [x, 2 * x ..] [y, 2 * y .. length m - 1]

countTrees m x y = length $ filter id $ map (uncurry (isTree m)) xys
    where xys = slope m x y
