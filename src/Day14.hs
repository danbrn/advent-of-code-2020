{-# LANGUAGE TupleSections #-}
-- | Advent of Code 2020 - Day 14

module Day14
    ( day14
    , day14'
    )
where

import           AoC

import qualified Data.IntMap.Strict            as M
import           Data.List.Split                ( splitOn )
import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                , isDigit
                                                )
import           Numeric                        ( showIntAtBase )
import           Data.List                      ( subsequences )

type Instruction = String
type Memory = M.IntMap Value
type Mask = String
type Position = Int
type Value = String

data Computer = Computer Memory Mask

day14 :: [String] -> Int
day14 = sum . map fromBinary . M.elems . memory . runProgram

day14' :: [String] -> Int
day14' = sum . map fromBinary . M.elems . memory . runProgram'

setMask :: Computer -> Mask -> Computer
setMask c = Computer (memory c)

applyMask :: Mask -> Value -> Value
applyMask m v = zipWith
    (\a b -> case a of
        'X' -> b
        '0' -> '0'
        '1' -> '1'
    )
    m
    v'
    where v' = replicate (36 - length v) '0' ++ v

memory :: Computer -> Memory
memory (Computer m _) = m

setMemory :: Computer -> Position -> Value -> Computer
setMemory (Computer mem msk) p v =
    Computer (M.insert p (applyMask msk v) mem) msk

runProgram :: [Instruction] -> Computer
runProgram = foldl runInstruction (Computer M.empty (replicate 36 '0'))

runInstruction :: Computer -> Instruction -> Computer
runInstruction c i = case take 4 i of
    "mask" -> setMask c $ drop 7 i
    "mem[" -> uncurry (setMemory c) (parseMem i)

parseMem :: Instruction -> (Position, Value)
parseMem i = (p, v)
  where
    si = splitOn " = " i
    p  = read . takeWhile isDigit . drop 4 $ head si
    v  = showIntAtBase 2 intToDigit (read (si !! 1)) ""

toBinary :: Value -> String
toBinary v = showIntAtBase 2 intToDigit (read v) ""

fromBinary :: Value -> Int
fromBinary v = sum $ zipWith debinarize (reverse v) [0 ..]
    where debinarize b p = digitToInt b * 2 ^ p

applyMask' :: Mask -> Position -> [Position]
applyMask' msk p = positions . foldl bitMagic (0, []) $ zip3 bitValues msk p'
  where
    p'        = replicate (36 - length p'') '0' ++ p'' where p'' = toBinary' p
    bitValues = [ 2 ^ n | n <- [35, 34 .. 0] ]
    bitMagic :: (Int, [Int]) -> (Int, Char, Char) -> (Int, [Int])
    bitMagic (v, fs) (bv, m, d) = case m of
        '1' -> (v + bv, fs)
        '0' -> if d == '1' then (v + bv, fs) else (v, fs)
        'X' -> (v, bv : fs)
    positions :: (Position, [Position]) -> [Position]
    positions (v, fs) =
        foldl (\acc fs' -> v + sum fs' : acc) [] (subsequences fs)

setMemory' :: Computer -> Position -> Value -> Computer
setMemory' (Computer mem msk) p v = Computer (M.union newValues mem) msk
    where newValues = M.fromList . map (, v) $ applyMask' msk p

toBinary' :: Position -> String
toBinary' p = showIntAtBase 2 intToDigit p ""

runProgram' :: [Instruction] -> Computer
runProgram' = foldl runInstruction' (Computer M.empty (replicate 36 '0'))

runInstruction' :: Computer -> Instruction -> Computer
runInstruction' c i = case take 4 i of
    "mask" -> setMask c $ drop 7 i
    "mem[" -> uncurry (setMemory' c) (parseMem i)
