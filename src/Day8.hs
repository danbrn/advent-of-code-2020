{-# LANGUAGE LambdaCase #-}
-- | Advent of Code 2020 - Day 8

module Day8
    ( day8
    , day8'
    )
where

import           AoC

import           Data.List                      ( isPrefixOf )
import qualified Data.IntMap                   as M

data Computer a = Computer Int Int (M.IntMap (Instr a)) (M.IntMap Int) Bool
    deriving Show

data Instr a = Nop | Acc a | Jmp a | Hlt
  deriving Show

day8 :: [String] -> Int
day8 input = accumulator $ runUntilRepeat computer
    where computer = initComputer input

day8' :: [String] -> Int
day8' input = accumulator . head . filter isDone $ map
    (\x -> runUntilRepeat (replaceInstr computer x Nop))
    (jmps computer)
  where
    computer = initComputer input
    jmps (Computer _ _ code _ _) = M.keys $ M.filter
        (\case
            (Jmp _) -> True
            _       -> False
        )
        code

initComputer input = (Computer 0 0 code (initExecuted code) False)
    where code = parse input

initExecuted = M.fromList . (flip zip) (cycle [0]) . M.keys

parse = M.fromList . zip [0 ..] . map parseOp

parseOp op | "nop" `isPrefixOf` op = Nop
           | "acc" `isPrefixOf` op = Acc arg
           | "jmp" `isPrefixOf` op = Jmp arg
           | otherwise             = Nop
    where arg = (read :: String -> Int) $ dropWhile (== '+') $ drop 4 op

runOne (Computer ip acc code exec False) = case code M.!? ip of
    Just Nop     -> (Computer (succ ip) acc code exec' False)
    Just (Acc x) -> (Computer (succ ip) (acc + x) code exec' False)
    Just (Jmp x) -> (Computer (ip + x) acc code exec' False)
    Nothing      -> (Computer ip acc code exec True)
    where exec' = M.adjust (+ 1) ip exec

runUntilRepeat comp@(Computer ip _ _ exec done) | done || n > 0 = comp
  where
    n = case exec M.!? ip of
        Just x -> x
        _      -> 0

runUntilRepeat comp = runUntilRepeat (runOne comp)

accumulator (Computer _ acc _ _ _) = acc
isDone (Computer _ _ _ _ done) = done

replaceInstr (Computer ip acc code exec done) offset instr =
    (Computer ip acc (M.update (\_ -> Just instr) offset code) exec done)
