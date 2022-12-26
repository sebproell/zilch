module Main where

import Zilch


main :: IO()
main = do
    putStrLn "Number of dice: "
    n <- getLine
    putStrLn "Score: "
    s <- getLine
    print $ mapForScore (read n) (read s)