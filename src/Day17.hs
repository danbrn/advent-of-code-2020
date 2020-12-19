{-# LANGUAGE TupleSections #-}
-- | Advent of Code 2020 - Day 17

module Day17
    ( day17
    , day17'
    )
where

import           AoC

import qualified Data.Set                      as S
import           Data.Set                       ( Set
                                                , (\\)
                                                )

type Coords = (Int, Int, Int)
type Grid = Set Coords

type Coords' = (Int, Int, Int, Int)
type Grid' = Set Coords'

input = lines ".#.\n..#\n###"
input' =
    lines
        ".......#\n....#...\n...###.#\n#...###.\n....##..\n##.#..#.\n###.#.#.\n....#..."

day17 :: [String] -> Int
day17 input = S.size $ iterate updateState grid !! 6
    where grid = parseInput input

day17' :: [String] -> Int
day17' input = S.size $ iterate updateState' grid !! 6
    where grid = parseInput' input

parseInput :: [String] -> Grid
parseInput =
    S.fromList
        . concat
        . zipWith (\r cs -> map (, r, 0) cs) [0 ..]
        . map parseRow

parseInput' :: [String] -> Grid'
parseInput' =
    S.fromList
        . concat
        . zipWith (\r cs -> map (, r, 0, 0) cs) [0 ..]
        . map parseRow

parseRow :: String -> [Int]
parseRow = map fst . filter snd . zip [0 ..] . map (== '#')

neighbors :: Coords -> Grid
neighbors (x, y, z) = S.fromList
    [ (x', y', z')
    | x' <- [x - 1 .. x + 1]
    , y' <- [y - 1 .. y + 1]
    , z' <- [z - 1 .. z + 1]
    , x' /= x || y' /= y || z' /= z
    ]

neighbors' :: Coords' -> Grid'
neighbors' (x, y, z, w) = S.fromList
    [ (x', y', z', w')
    | x' <- [x - 1 .. x + 1]
    , y' <- [y - 1 .. y + 1]
    , z' <- [z - 1 .. z + 1]
    , w' <- [w - 1 .. w + 1]
    , x' /= x || y' /= y || z' /= z || w' /= w
    ]

stayAwake :: Grid -> Coords -> Bool
stayAwake grid = flip elem [2, 3] . S.size . S.intersection grid . neighbors

stayAwake' :: Grid' -> Coords' -> Bool
stayAwake' grid = flip elem [2, 3] . S.size . S.intersection grid . neighbors'

wakeUp :: Grid -> Coords -> Bool
wakeUp grid = (==) 3 . S.size . S.intersection grid . neighbors

wakeUp' :: Grid' -> Coords' -> Bool
wakeUp' grid = (==) 3 . S.size . S.intersection grid . neighbors'

updateState :: Grid -> Grid
updateState grid = stayingAwake `S.union` wakingUp
  where
    toCheck      = S.unions (S.map neighbors grid) \\ grid
    stayingAwake = S.filter (stayAwake grid) grid
    wakingUp     = S.filter (wakeUp grid) toCheck

updateState' :: Grid' -> Grid'
updateState' grid = stayingAwake `S.union` wakingUp
  where
    toCheck      = S.unions (S.map neighbors' grid) \\ grid
    stayingAwake = S.filter (stayAwake' grid) grid
    wakingUp     = S.filter (wakeUp' grid) toCheck
