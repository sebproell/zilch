module Main where

import Zilch

roll:: Int -> Score -> Int -> IO ()
roll n s i = do
    putStrLn $ "Unconditionally rolling "++ show i ++
        " times gives " ++ (show . expectedNode s i . makeGraph) n ++
        " on average --> " ++ (show . recommend s i . makeGraph) n

loop :: IO ()
loop = do
    putStrLn "Number of dice: "
    n <- getLine
    putStrLn "Score: "
    s <- getLine
    roll (read n) (read s) 1
    roll (read n) (read s) 2
    roll (read n) (read s) 3
    loop

main :: IO()
main = loop