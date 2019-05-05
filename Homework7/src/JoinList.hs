module JoinList where

import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single x _)   = x
tag (Append x _ _) = x

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

type SizeList a = JoinList Size a

indexJ :: Size -> SizeList a -> Maybe a
indexJ _ Empty            = Nothing
indexJ i _ | i < 0        = Nothing
indexJ i x | (tag x) <= i = Nothing
indexJ 0 (Single _ x)     = Just x
indexJ i (Append _ a b)
  | sizeA > i = indexJ i a
  | otherwise = indexJ (i - sizeA) b
  where
    sizeA = tag a
  
dropJ :: Size -> SizeList a -> SizeList a
dropJ 0 x                = x
dropJ i x | (tag x) <= i = Empty
dropJ _ Empty            = Empty
dropJ i (Append s a b)
  | sizeA > i = Append sizeR (dropJ i a) b
  | otherwise = Append sizeR a (dropJ (i - sizeA) b)
  where
    sizeR = s - i
    sizeA = tag a

takeJ :: Size -> SizeList a -> SizeList a
takeJ 0 x                = Empty
takeJ _ Empty            = Empty
takeJ i x | (tag x) <= i = x
takeJ i (Append s a b)
  | sizeA > i = takeJ i a
  | otherwise = Append i a (takeJ (i - sizeA) b)
  where
    sizeA = tag a

type SizeListBuffer = SizeList String

{-
instance Buffer SizeListBuffer where
    toString   = unlines . jlToList
    fromString = 
      -}

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x