{-# LANGUAGE TupleSections #-}
-- | Advent of Code 2020 - Day 1

module Day1
    ( bruteDay1
    , bruteDay1'
    , day1
    , day1'
    )
where

import           AoC
import qualified Data.IntSet                   as S
import           Data.List                      ( tails )

day1 xs = x * (target - x)
  where
    target = 2020
    x      = findMatch xs' target set id
    xs'    = map read xs
    set    = S.fromList xs'

day1' xs = x * y * (target - x - y)
  where
    target = 2020
    (x, y) = findMatch xys target set (uncurry (+))
    xys    = filter (\(x, y) -> x + y < target) (makePairs xs')
    xs'    = map read xs
    set    = S.fromList xs'

makePairs = concatMap makePairs' . tails
  where
    makePairs' []       = []
    makePairs' (x : xs) = map (x, ) xs

findMatch :: [a] -> Int -> S.IntSet -> (a -> Int) -> a
findMatch [] _ _ _ = error "not found"
findMatch (x : xs) target set f =
    if S.member (target - f x) set then x else findMatch xs target set f

-- Brute force variants.
bruteDay1 xs = head [ x * y | x <- xs', y <- xs', x + y == 2020 ]
    where xs' = map read xs

bruteDay1' xs = head
    [ x * y * z | x <- xs', y <- xs', z <- xs', x + y + z == 2020 ]
    where xs' = map read xs
