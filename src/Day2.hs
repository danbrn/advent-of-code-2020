-- | Advent of Code 2020 - Day 2

module Day2
    ( day2
    , day2'
    )
where

import           AoC
import           Data.Char                      ( isDigit
                                                , isLetter
                                                , isSpace
                                                )

day2 :: [String] -> Int
day2 = length . filter id . map verifyLine

day2' :: [String] -> Int
day2' = length . filter id . map verifyLine'

parseLine :: String -> (Int, Int, Char, String)
parseLine l = (minCh, maxCh, ch, pw)
  where
    minCh = read $ takeWhile isDigit l
    maxCh = read $ drop 1 $ dropWhile isDigit $ takeWhile (not . isSpace) l
    ch    = head $ dropWhile (not . isLetter) $ takeWhile (/= ':') l
    pw    = drop 2 $ dropWhile (/= ':') l

verifyLine :: String -> Bool
verifyLine l = numCh >= minCh && numCh <= maxCh
  where
    (minCh, maxCh, ch, pw) = parseLine l
    numCh                  = length $ filter (== ch) pw

xor :: Bool -> Bool -> Bool
b `xor` True  = not b
b `xor` False = b

verifyLine' :: String -> Bool
verifyLine' l = (pw !! (minCh - 1) == ch) `xor` (pw !! (maxCh - 1) == ch)
    where (minCh, maxCh, ch, pw) = parseLine l
