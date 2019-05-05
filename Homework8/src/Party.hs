module Party where

import Data.Monoid
import Data.Semigroup
import Data.Tree

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL list fun) = GL (e:list) (fun + empFun e)

instance Semigroup GuestList where
  (<>) (GL aList aFun) (GL bList bFun) = GL (aList ++ bList) (aFun + bFun)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold base f node = f (rootLabel node) (map (treeFold base f) (subForest node))