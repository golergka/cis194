{-# LANGUAGE FlexibleInstances #-}

module Homework6.Fibonacci where

import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n 
    | n > 0 = (fib (n - 2)) + (fib (n - 1))
    | n < 0 = (fib (n + 2)) - (fib (n + 1))

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibPairNext :: (Integer, Integer) -> (Integer, Integer)
fibPairNext (x, y) = (y, x + y)

fibPairFirst = (0, 1)

fibs2 :: [Integer]
fibs2 = map fst (iterate fibPairNext fibPairFirst)

data Stream a = Element a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Element x y) = x:(streamToList y)

instance Show a => Show (Stream a) where
    show x = show (take 20 (streamToList x))

streamRepeat :: a -> Stream a
streamRepeat x = Element x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Element x y) = Element (f x) (streamMap f y)

streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZipWith f (Element a0 a') (Element b0 b') = Element (f a0 b0) (streamZipWith f a' b')

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Element x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed ((+) 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Element a b) c = Element a (interleaveStreams c b)

rulerN :: Integer -> Stream Integer
rulerN n = interleaveStreams (streamRepeat n) (rulerN (n + 1))

ruler :: Stream Integer
ruler = rulerN 0

instance Num (Stream Integer) where

    fromInteger x = Element x (streamRepeat 0)

    negate x = streamMap negate x

    (+) = streamZipWith (+)

    (*) (Element a0 a') (Element b0 b') = Element (a0 * b0) (Element (streamMap (* a0) b') (a' * (Element b0 b')))