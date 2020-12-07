-- | Advent of Code 2020 - Day 7

module Day7
    ( day7
    , day7'
    )
where

import           AoC

import qualified Data.Map.Strict               as M
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( fromJust )
import           Data.List                      ( nub )

day7 :: [String] -> Int
day7 input = length $ filter id $ map (contains m "shiny gold") $ M.keys m
    where m = M.map nub $ parseRules input

day7' :: [String] -> Int
day7' input = length $ tail $ bags m "shiny gold" where m = parseRules input

parseRules :: [String] -> M.Map String [String]
parseRules = M.fromList . map parseBag

parseBag :: String -> (String, [String])
parseBag rule = (bag, parseContents $ head contents)
  where
    (bag, contents) =
        (\xs -> (head xs, tail xs)) $ splitOn " bags contain " rule

parseContents :: String -> [String]
parseContents ""               = []
parseContents "no other bags." = []
parseContents contents         = concatMap (multiple . init . words)
    $ splitOn ", " contents
    where multiple (n : xs) = replicate (read n) (unwords xs)

contains :: M.Map String [String] -> String -> String -> Bool
contains m b bs | b == bs   = False
                | otherwise = contains' m b bs
  where
    contains' m b bs | b == bs   = True
                     | null bs'  = False
                     | otherwise = any (contains' m b) bs'
        where bs' = fromJust $ M.lookup bs m

bags :: M.Map String [String] -> String -> [String]
bags m b = b : concatMap (bags m) (fromJust $ M.lookup b m)
