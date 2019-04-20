module Homework3.Golf where

import Data.List
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

nonEmpty :: String -> Bool
nonEmpty = any ((/=) ' ')

histCountN :: Int -> [Int] -> Int
histCountN n = length . filter ((==) n)

histDisplayColumn :: [Int] -> Int -> String
histDisplayColumn xs n = show n ++ "=" ++ replicate (histCountN n xs) '*' ++ repeat ' '

histDisplayLinesRev :: [Int] -> [String]
histDisplayLinesRev xs = takeWhile nonEmpty $ transpose $ map (histDisplayColumn xs) [0..9]

histogram :: [Int] -> String
histogram = unlines . reverse . histDisplayLinesRev