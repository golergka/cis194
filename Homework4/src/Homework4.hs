module Homework4 where

import Data.List

fun1 :: [Integer] -> Integer
{-
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs
-}
fun1 xs = product $ map ((-) 2) $ filter even xs

{-
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n 'div' 2)
       | otherwise = fun2 (3 * n + 1)
-}

-- fun2 x = takeWhile even $ iterate (flip div 2) x

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

heightTo :: Tree a -> Integer
heightTo Leaf = 0
heightTo (Node x _ _ _) = x + 1

makeNode :: Tree a -> a -> Tree a -> Tree a
makeNode l c r = Node (max (heightTo l) (heightTo r)) l c r

add :: Tree a -> a -> Tree a
add Leaf x = Node 0 Leaf x Leaf
add (Node i l x r) y
    | heightTo l < heightTo r   = makeNode (add l y) x r
    | otherwise                 = makeNode l x (add r y)

foldTree :: [a] -> Tree a
foldTree = foldl add Leaf

xor :: [Bool] -> Bool
xor = foldl (/=) False

makeMapFold :: (a -> b) -> [b] -> a -> [b]
makeMapFold f xs y = (f y):xs

map' :: (a -> b) -> [a] -> [b]
map' f x = foldl (makeMapFold f) [] x

sieveSundaramRem :: Integer -> [Integer]
sieveSundaramRem n = [i + j + 2 * i * j | i <- [1..n], j <- [1..n], i <= j]

-- |sieveSundaram should generate all the odd primes up to 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) $ [1..n] \\ sieveSundaramRem n