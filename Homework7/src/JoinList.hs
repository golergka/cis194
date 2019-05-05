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

(+++) :: Monoid m => 
         JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

instance (Sized m, Monoid m) => Sized (JoinList m a) where
  size = size . tag

indexJ :: (Sized m, Monoid m) =>
          Size -> JoinList m a -> Maybe a
indexJ _ Empty            = Nothing
indexJ i _ | i < 0        = Nothing
indexJ i x | (size x) <= i = Nothing
indexJ 0 (Single _ x)     = Just x
indexJ i (Append _ a b)
  | sizeA > i = indexJ i a
  | otherwise = indexJ (i - sizeA) b
  where
    sizeA = size a
  
dropJ :: (Sized m, Monoid m) =>
         Size -> JoinList m a -> JoinList m a
dropJ 0 x                = x
dropJ i x | (size x) <= i = Empty
dropJ _ Empty            = Empty
dropJ i (Append s a b)
  | size a > i = 
      let l = dropJ i a
        in Append ((tag l) <> (tag b)) l b
  | otherwise = dropJ (i - (size a)) b

takeJ :: (Sized m, Monoid m) =>
         Size -> JoinList m a -> JoinList m a
takeJ 0 x                = Empty
takeJ _ Empty            = Empty
takeJ i x | (size x) <= i = x
takeJ i (Append s a b)
  | (size a) > i = takeJ i a
  | otherwise =
      let r = takeJ (i - (size a)) b
        in Append ((tag a) <> (tag r)) a r

type SizeListBuffer = JoinList Size String

{-
instance Buffer SizeListBuffer where
    toString   = unlines . jlToList
    fromString = 
      -}

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x