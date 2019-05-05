module JoinList where

import Sized

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

type SizeList a = JoinList Size a

indexJ :: Size -> SizeList a -> Maybe a
indexJ _ Empty                       = Nothing
indexJ i _ | i < 0                   = Nothing
indexJ i x | (tag x) <= i = Nothing
indexJ 0 (Single _ x)                = Just x
indexJ i (Append _ a b)
  | sizeA > i = indexJ i a
  | otherwise = indexJ (i - sizeA) b
  where
    sizeA = tag a
  
dropJ :: Size -> SizeList a -> SizeList a
dropJ 0 x                           = x
dropJ i x | (tag x) <= i = Empty
dropJ _ Empty                       = Empty
dropJ _ (Single _ _)                = Empty
dropJ i (Append s a b)
  | sizeA > i = Append (s - i) (dropJ i a) b
  | otherwise = Append (s - i) a (dropJ (i - sizeA) b)
  where
    sizeA = tag a