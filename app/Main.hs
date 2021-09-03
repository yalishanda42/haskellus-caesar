module Main where

import DCD

main :: IO ()
main = do
    str <- getLine
    key <- getLine
    let keyInt = read key :: Int
    putStrLn $ encode str keyInt
