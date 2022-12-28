module Main where

import Zilch

loop :: IO ()
loop = do
    putStrLn "Number of dice: "
    n <- getLine
    putStrLn "Score: "
    s <- getLine
    putStr "Direct expected result: "
    print $ expectedNode 1 . makeGraph $ read n
    putStr "2nd-level expected result: "
    print $ expectedNode 2 . makeGraph $ read n
    putStr "3rd-level expected result: "
    print $ expectedNode 3 . makeGraph $ read n
    loop

main :: IO()
main = loop