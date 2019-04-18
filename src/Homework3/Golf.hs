module Homework3.Golf where

import Debug.Trace

skipsN :: [a] -> Int -> [a]
skipsN xs n = [fst i | i <- (zip xs [1..]), (mod (snd i) n) == 0]

skips :: [a] -> [[a]]
skips xs = map (skipsN xs) [1..(length xs)]

localMaxima :: [Int] -> [Int]
localMaxima (x:(y:(z:ws)))
    | (y > x) && (y > z) = y:(localMaxima (z:ws))
    | otherwise = localMaxima (y:(z:ws))
localMaxima _ = []

histDisplayPoint :: Int -> Int -> Char
histDisplayPoint row value
    | value >= row = '*'
    | otherwise = ' '

histDisplayRow :: [Int] -> Int -> String
histDisplayRow values row = map (histDisplayPoint row) values

histDisplayLines :: [Int] -> [String]
histDisplayLines values = (map (histDisplayRow values) [1..])

histDisplayLegend :: Int -> [String]
histDisplayLegend x = (take x (['0'..])):(take x (repeat '=')):[]

nonEmpty :: String -> Bool
nonEmpty = any ((/=) ' ')

histDisplay :: [Int] -> String
histDisplay values = (unlines . reverse . ((++) (histDisplayLegend 10))) (takeWhile nonEmpty (histDisplayLines values))

histCountN :: Int -> [Int] -> Int
histCountN n = (length . filter ((==) n))

histCount :: [Int] -> [Int]
histCount xs = map ((flip histCountN) xs) [0..9]

histogram :: [Int] -> String
histogram = histDisplay . histCount