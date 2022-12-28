module Main where

import Zilch

loop :: IO ()
loop = do
    putStrLn "Number of dice: "
    n <- getLine
    putStrLn "Score: "
    s <- getLine
    putStr "Direct expected result: "
    print $ expectedTree 1 . makeTree $ GameState (read s) (read n)
    putStr "2nd-level expected result: "
    print $ expectedTree 2 . makeTree $ GameState (read s) (read n)
    putStr "3rd-level expected result: "
    print $ expectedTree 3 . makeTree $ GameState (read s) (read n)
    loop

main :: IO()
main = loop