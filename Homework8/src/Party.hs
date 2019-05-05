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

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f node = f (rootLabel node) (map (treeFold f) (subForest node))

-- |first guest list is a variant with a boss, second - without
combineGLs :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
combineGLs boss []     = (glCons boss base, base)
  where
    base = GL [] 0
combineGLs boss combos = (maximum (map (glCons boss) allCombos), (maximum allCombos))
  where
    (combosW, combosWO) = unzip combos
    allCombos = combosW ++ combosWO

maxFun :: Tree Employee -> GuestList
maxFun company = max a b
  where (a, b) = treeFold combineGLs company