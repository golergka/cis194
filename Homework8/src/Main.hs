module Main where

import Party

showMaxFun :: String -> String
showMaxFun = show . maxFun . read

main :: IO ()
main = putStrLn "Reading from file..." >> 
       readFile "company.txt" >>= 
       putStrLn . showMaxFun