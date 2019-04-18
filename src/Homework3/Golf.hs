module Homework3.Golf where

import Debug.Trace

{-
skipsNRec :: [a] -> Int -> Int -> [a]
skipsNRec [] _ _ = []
skipsNRec (x:ys) n 1 = x:(skipsNRec ys n n)
skipsNRec (x:ys) n i = skipsNRec ys n (i - 1)

skipsN :: [a] -> Int -> [a]
skipsN xs n = skipsNRec xs n n
-}

isDiv :: Int -> Int -> Bool
isDiv x y = (mod y x) == 0

skipsN :: [a] -> Int -> [a]
skipsN xs n = fst (unzip (filter ((isDiv n).snd) (zip xs [1..(length xs)])))

skips :: [a] -> [[a]]
skips xs = map (skipsN xs) [1..(length xs)]