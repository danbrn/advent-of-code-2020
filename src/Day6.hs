-- | Advent of Code 2020 - Day 6

module Day6
    ( day6
    , day6'
    )
where

import           AoC
import           Day4                           ( chopList )

import qualified Data.Set                      as S
import           Data.Char                      ( isSpace )

day6 :: [String] -> Int
day6 = sum . map (S.size . collateGroup) . chopList

day6' :: [String] -> Int
day6' = sum . map (length . collateGroup') . chopList

collateGroup = S.fromList . filter (not . isSpace) . unlines

collateGroup' ls = filter allYes ['a' .. 'z']
  where
    as = map S.fromList ls
    allYes c = all (c `S.member`) as
