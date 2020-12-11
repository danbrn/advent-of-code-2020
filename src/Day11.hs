-- | Advent of Code 2020 - Day 11

module Day11
    ( day11
    , day11'
    )
where

import           AoC

import qualified Data.Map                      as M
import           Data.Maybe                     ( isNothing
                                                , mapMaybe
                                                )

data Seat = Empty | Taken | None
  deriving Eq

instance Show Seat where
    show Empty = "L"
    show Taken = "#"
    show None  = "."

type Position = (Int, Int)

data SeatingMap = SeatingMap
    { rows    :: Int
    , columns :: Int
    , seats   :: M.Map Position Seat
    }
    deriving Eq

input =
    lines
        "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"

day11 :: [String] -> Int
day11 =
    length . filter (== Taken) . M.elems . seats . untilStable rule . buildMap

day11' :: [String] -> Int
day11' =
    length . filter (== Taken) . M.elems . seats . untilStable rule' . buildMap

buildMap :: [String] -> SeatingMap
buildMap input = SeatingMap (length input)
                            (length $ head input)
                            (M.fromList $ parseInput input)

parseInput :: [String] -> [(Position, Seat)]
parseInput =
    concat
        . zipWith (\r xs -> map (\(c, x) -> ((r, c), toSeat x)) xs) [0 ..]
        . map parseLine

parseLine :: String -> [(Int, Char)]
parseLine = filter (\(_, x) -> x /= '.') . zip [0 ..]

toSeat :: Char -> Seat
toSeat 'L' = Empty
toSeat '#' = Taken
toSeat '.' = None
toSeat _   = error "illegal character"

neighbors :: SeatingMap -> Position -> Int
neighbors m p = length . filter (== Taken) $ mapMaybe
    (\k -> M.lookup k (seats m))
    neighbors'
  where
    neighbors' =
        [ (r, c)
        | r <- [fst p - 1 .. fst p + 1]
        , r >= 0
        , r < rows m
        , c <- [snd p - 1 .. snd p + 1]
        , c >= 0
        , c < columns m
        , r /= fst p || c /= snd p
        ]

rule :: SeatingMap -> Position -> Seat
rule m p | isNothing s     = None
         | s == Just Taken = if neighbors m p >= 4 then Empty else Taken
         | s == Just Empty = if neighbors m p == 0 then Taken else Empty
    where s = M.lookup p $ seats m

canSee :: SeatingMap -> Position -> Int
canSee m p = length . filter (== Taken) $ map (look m p) dirs
  where
    dirs =
        [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

look m (r, c) _ | r < 0 || r >= rows m || c < 0 || c >= columns m = None
look m (r, c) (r', c') | s == Just Taken = Taken
                       | s == Just Empty = Empty
                       | otherwise       = look m (r + r', c + c') (r', c')
    where s = M.lookup (r + r', c + c') $ seats m

rule' :: SeatingMap -> Position -> Seat
rule' m p | isNothing s     = None
          | s == Just Taken = if canSee m p >= 5 then Empty else Taken
          | s == Just Empty = if canSee m p == 0 then Taken else Empty
    where s = M.lookup p $ seats m

applyRule r m = SeatingMap
    (rows m)
    (columns m)
    (M.fromList $ map (\x -> (x, r m x)) $ M.keys $ seats m)

untilStable r m | m == m'   = m
                | otherwise = untilStable r m'
    where m' = applyRule r m
