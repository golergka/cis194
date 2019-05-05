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

instance (Sized b, Monoid b) => Sized (JoinList b a) where
  size jl = (size . tag) jl

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty                       = Nothing
indexJ i _ | i < 0                   = Nothing
indexJ i x | (getSize $ size x) <= i = Nothing
indexJ 0 (Single _ x)                = Just x
indexJ i (Append _ a b)
  | sizeA > i = indexJ i a
  | otherwise = indexJ (i - sizeA) b
  where
    sizeA = getSize $ size a