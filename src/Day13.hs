-- | Advent of Code 2020 - Day 13

module Day13
    ( day13
    , day13'
    )
where

import           AoC
import           Data.List.Split                ( splitOn )

day13 :: [String] -> Int
day13 input = uncurry (*) mt
  where
    t = read $ head input
    ts =
        map ((\x -> (head (dropWhile (< t) [0, x ..]) - t, x)) . read)
            .  filter (/= "x")
            .  splitOn ","
            $  input
            !! 1
    mt = minimum ts


-- day13' :: [String] -> Int
day13' input = crt bs
  where
    bs =
        map (\(m, k') -> let k = read k' in (m `mod` k, k))
            .  filter (\(_, k) -> k /= "x")
            .  zip [0 ..]
            .  splitOn ","
            $  input
            !! 1

firstPositive (a, m) | a >= 0    = (m - a) `mod` m
                     | a < (-m)  = m - a `mod` m
                     | otherwise = a `mod` m

crt2 (a, m1) (b, m2) = (ab, m)
  where
    ab     = a * m2 * v + b * m1 * u
    m      = m1 * m2
    (u, v) = pulverizer m1 m2

crt :: [(Integer, Integer)] -> (Integer, Integer)
crt = foldl1 crt2

pulverizer x y = (signum x * fst res, signum y * snd res)
  where
    pulverizer' a b = head
        $ dropWhile (not . done) (iterate step (1, 0, a, b, 0, 1))
      where
        d = gcd a b
        done (_, _, a, b, _, _) = a == d || b == d
        step (u1, v1, a, b, u2, v2)
            | a <= b    = (u1, v1, a, bma, u2 - bda * u1, v2 - bda * v1)
            | otherwise = (u1 - adb * u2, v1 - adb * v2, amb, b, u2, v2)
          where
            amb = a `mod` b
            bma = b `mod` a
            adb = a `div` b
            bda = b `div` a
    (u1, v1, p, _, u2, v2) = pulverizer' (abs x) (abs y)
    res | p == gcd x y = (u1, v1)
        | otherwise    = (u2, v2)
