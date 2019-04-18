module Homework3.Golf where

import Debug.Trace

skipsN :: [a] -> Int -> [a]
skipsN xs n = [fst i | i <- (zip xs [1..]), (mod (snd i) n) == 0]

skips :: [a] -> [[a]]
skips xs = map (skipsN xs) [1..(length xs)]