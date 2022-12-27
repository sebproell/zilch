module Main where

import Zilch


main :: IO()
main = do
    putStrLn "Number of dice: "
    n <- getLine
    putStrLn "Score: "
    s <- getLine
    print $ sum . map maximum . (map . map) expected . recurse $ GameState (read s) (read n)