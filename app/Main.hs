module Main where

import Zilch


logo :: String
logo = ">=======>>=>      >=>                  \n" ++
       "       >=>    >>  >=>         >=>      \n" ++
       "      >=>         >=>    >==> >=>      \n" ++
       "    >=>      >=>  >=>  >=>    >=>>=>   \n" ++
       "   >=>       >=>  >=> >=>     >=>  >=> \n" ++
       " >=>         >=>  >=>  >=>    >>   >=> \n" ++
       ">==========> >=> >==>    >==> >=>  >=> \n"

info :: Int -> AnnotatedScore -> IO()
info i (s, Take) = putStrLn $ "Rolling up to "++ show i ++
        " times (when reasonable) gives " ++ show s ++
        " on average --> Take another turn"
info i (s, Reject) = putStrLn $ "Rolling up to "++ show i ++
        " times (when reasonable) never gives more points than you already have" ++
        " --> Stop"

roll:: Int -> Score -> Int -> IO ()
roll n s i = do
    let expected = expectedNode s i . makeGraph $ n in
        info i expected

loop :: IO ()
loop = do
    putStrLn "Number of dice: "
    n <- getLine
    putStrLn "Score: "
    s <- getLine
    roll (read n) (read s) 1
    roll (read n) (read s) 2
    roll (read n) (read s) 3
    roll (read n) (read s) 4
    loop

main :: IO()
main = do
    putStrLn logo
    loop