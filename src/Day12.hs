-- | Advent of Code 2020 - Day 12

module Day12
    ( day12
    , day12'
    )
where

import           AoC

data Ferry = Ferry Direction Position
    deriving Show

data Direction = North | East | South | West

type Position = (Int, Int)

type Waypoint = (Int, Int)

type Command = (Char, Int)

type Route = [Command]

data Ferry' = Ferry' Waypoint Position
    deriving Show

instance Show Direction where
    show North = "N"
    show East  = "E"
    show South = "S"
    show West  = "W"

instance Enum Direction where
    toEnum 0 = North
    toEnum 1 = East
    toEnum 2 = South
    toEnum 3 = West
    toEnum x = toEnum $ x `mod` 4

    fromEnum North = 0
    fromEnum East  = 1
    fromEnum South = 2
    fromEnum West  = 3

day12 :: [String] -> Int
day12 = manhattan . position . navigate (Ferry East (0, 0)) . parseRoute

day12' :: [String] -> Int
day12' =
    manhattan . position' . navigate' (Ferry' (10, -1) (0, 0)) . parseRoute

manhattan (x, y) = abs x + abs y
parseRoute :: [String] -> [Command]
parseRoute = map (\(cmd : arg) -> (cmd, read arg))

navigate :: Ferry -> Route -> Ferry
navigate = foldl navigate'
  where
    navigate' :: Ferry -> Command -> Ferry
    navigate' f@(Ferry dir p@(x, y)) (cmd, arg) = case cmd of
        'F' -> navigate' f (head $ show dir, arg)
        'N' -> Ferry dir (x, y - arg)
        'E' -> Ferry dir (x + arg, y)
        'S' -> Ferry dir (x, y + arg)
        'W' -> Ferry dir (x - arg, y)
        'L' -> Ferry (toEnum (fromEnum dir - arg `div` 90)) p
        'R' -> Ferry (toEnum (fromEnum dir + arg `div` 90)) p
        _   -> error "illegal command"

position :: Ferry -> Position
position (Ferry _ p) = p

position' :: Ferry' -> Position
position' (Ferry' _ p) = p

waypoint :: Ferry' -> Waypoint
waypoint (Ferry' w _) = w

navigate' :: Ferry' -> Route -> Ferry'
navigate' = foldl navigate''
  where
    navigate'' (Ferry' w@(wx, wy) p@(px, py)) (cmd, arg) = case cmd of
        'F' -> Ferry' w (px + arg * wx, py + arg * wy)
        'N' -> Ferry' (wx, wy - arg) p
        'E' -> Ferry' (wx + arg, wy) p
        'S' -> Ferry' (wx, wy + arg) p
        'W' -> Ferry' (wx - arg, wy) p
        'L' -> Ferry' (turn left' w arg) p
        'R' -> Ferry' (turn right' w arg) p

left' (x, y) _ = (y, -x)

right' (x, y) _ = (-y, x)

turn f w d = foldl f w [1 .. (d `div` 90)]
