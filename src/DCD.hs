{- Ceaser cipher encoding and decoding functions. -}

module DCD
    ( encode
    , decode
    ) where

import Data.List
import Data.Maybe

lowercases = ['a'..'z']
uppercases = ['A'..'Z']

elementIndex :: Char -> [Char] -> Int
elementIndex el list = fromJust $ elemIndex el list

transposeLetter :: Char -> Int -> Char
transposeLetter char amount
    | char `elem` lowercases = cycle lowercases !! ((elementIndex char lowercases) + amount)
    | char `elem` uppercases = cycle uppercases !! ((elementIndex char uppercases) + amount)
    | otherwise = char

encodeLetter :: Char -> Int -> Char
encodeLetter char key
    | key <= 1 || key >= 27 = char
    | otherwise = transposeLetter char (key - 1)

decodeLetter :: Char -> Int -> Char
decodeLetter char key
    | key <= 1 || key >= 27 = char
    | otherwise = transposeLetter char (-(key - 1))

encode :: String -> Int -> String
encode str key = [encodeLetter c key | c <- str]

decode :: String -> Int -> String
decode str key = [decodeLetter c key | c <- str]

