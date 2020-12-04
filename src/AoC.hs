-- | Utility functions.

module AoC
    ( apply
    , applyN
    )
where

import           Control.Monad                  ( forM_ )

apply f fn = do
    input <- lines <$> readFile fn
    putStrLn $ show $ f $ input

applyN :: ([String] -> Int) -> String -> Int -> IO ()
applyN f fn n = do
    input <- lines <$> readFile fn
    forM_ [1 .. n] (\_ -> seq (f input) (return ()))
