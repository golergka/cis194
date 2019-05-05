{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Data.Semigroup

import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => 
       JoinList m a -> m
tag Empty          = mempty
tag (Single x _)   = x
tag (Append x _ _) = x

(+++) :: Monoid m => 
         JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

instance Monoid m =>
         Semigroup (JoinList m a) where
  (<>) = (+++)

instance Monoid m =>
         Monoid (JoinList m a) where
  mempty = Empty

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

instance (Sized m, Monoid m) => 
         Sized (JoinList m a) where
  size = size . tag

indexJ :: (Sized m, Monoid m) =>
          Size -> JoinList m a -> Maybe a
indexJ _ Empty            = Nothing
indexJ i _ | i < 0        = Nothing
indexJ i x | (size x) <= i = Nothing
indexJ 0 (Single _ x)     = Just x
indexJ i (Append _ a b)
  | (size a) > i = indexJ i a
  | otherwise    = indexJ (i - (size a)) b
  
dropJ :: (Sized m, Monoid m) =>
         Size -> JoinList m a -> JoinList m a
dropJ 0 x                = x
dropJ i x | (size x) <= i = Empty
dropJ _ Empty            = Empty
dropJ i (Append s a b)
  | size a > i = (dropJ i a) +++ b
  | otherwise  = dropJ (i - (size a)) b

takeJ :: (Sized m, Monoid m) =>
         Size -> JoinList m a -> JoinList m a
takeJ 0 x                = Empty
takeJ _ Empty            = Empty
takeJ i x | (size x) <= i = x
takeJ i (Append s a b)
  | (size a) > i = takeJ i a
  | otherwise    = a +++ (takeJ (i - (size a)) b)

scoreLine :: String -> JoinList (Score, Size) String
scoreLine x = Single (scoreString x, 1) x

instance Buffer (JoinList (Score, Size) String) where
  toString Empty          = ""
  toString (Single _ a)   = a
  toString (Append _ x y) = (toString x) ++ (toString y)

  fromString x = mconcat (map scoreLine (lines x))

  line i = indexJ (Size i)

  replaceLine i line list = before +++ (scoreLine line) +++ after
    where 
      before  = takeJ (Size i) list
      after   = dropJ (Size (i + 1)) list

  numLines = getSize . size

  value x = getScore (fst (tag x))