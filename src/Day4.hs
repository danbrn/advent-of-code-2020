-- | Advent of Code 2020 - Day 4

module Day4
    ( day4
    , day4'
    , chopList
    )
where

import           AoC

import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           Data.Char                      ( isHexDigit
                                                , isDigit
                                                )

type Passport = M.Map String String

day4 :: [String] -> Int
day4 = validateInput present

day4' :: [String] -> Int
day4' = validateInput valid

validateInput f = length . filter f . map parsePassport . chopList

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

present :: Passport -> Bool
present p = all (\x -> isJust $ M.lookup x p) requiredFields

parsePassport :: [String] -> Passport
parsePassport =
    M.fromList . map ((\[x, y] -> (x, y)) . splitOn ":") . words . unlines

allowedEyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

valid :: Passport -> Bool
valid p = all validValue requiredFields
  where
    validValue k = isJust v' && case k of
        "byr" -> validYear v 1920 2002
        "iyr" -> validYear v 2010 2020
        "eyr" -> validYear v 2020 2030
        "hgt" -> validHeight v
        "hcl" -> validHairColor v
        "ecl" -> v `elem` allowedEyeColors
        "pid" -> validPid v
      where
        v  = fromJust v'
        v' = M.lookup k p
    validHeight v = case u of
        "cm" -> h >= 150 && h <= 193
        "in" -> h >= 59 && h <= 76
        _    -> False
      where
        h = read $ takeWhile isDigit v
        u = dropWhile isDigit v
    validHairColor c =
        prefix == '#' && length colorCode == 6 && all isHexDigit colorCode
      where
        prefix    = head c
        colorCode = tail c
    validYear y from to = all isDigit y && y' >= from && y' <= to
        where y' = read y
    validPid p = length p == 9 && all isDigit p && read p >= 0
