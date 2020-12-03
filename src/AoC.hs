-- | Utility functions.

module AoC
    ( apply
    )
where

apply f fn = do
    input <- lines <$> readFile fn
    putStrLn $ show $ f $ input
