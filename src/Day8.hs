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

data Instr a = Nop a | Acc a | Jmp a
  deriving Show

day8 :: [String] -> Int
day8 input = accumulator $ runUntilRepeat computer
    where computer = initComputer input

day8' :: [String] -> Int
day8' input = accumulator . head . filter isDone $ map
    (\x -> runUntilRepeat (switchNopJmp computer x))
    (instrs computer)
  where
    computer = initComputer input
    instrs (Computer _ _ code _ _) = M.keys $ M.filter
        (\case
            (Jmp _) -> True
            (Nop _) -> True
            _       -> False
        )
        code

initComputer input = (Computer 0 0 code (initExecuted code) False)
    where code = parse input

initExecuted = M.fromList . (flip zip) (cycle [0]) . M.keys

parse = M.fromList . zip [0 ..] . map parseOp

parseOp op | "nop" `isPrefixOf` op = Nop arg
           | "acc" `isPrefixOf` op = Acc arg
           | "jmp" `isPrefixOf` op = Jmp arg
           | otherwise             = error "illegal instruction"
    where arg = (read :: String -> Int) $ dropWhile (== '+') $ drop 4 op

runOne (Computer ip acc code exec False) = case code M.!? ip of
    Just (Nop _) -> (Computer (succ ip) acc code exec' False)
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

switchNopJmp (Computer ip acc code exec done) offset =
    (Computer
        ip
        acc
        (M.adjust
            (\case
                (Nop x) -> (Jmp x)
                (Jmp x) -> (Nop x)
                instr   -> instr
            )
            offset
            code
        )
        exec
        done
    )
