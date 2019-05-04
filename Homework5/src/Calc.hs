{-# LANGUAGE FlexibleInstances #-}
module Calc where

import qualified Data.Map as M

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x)    = x
eval (Add x y)  = (eval x) + (eval y)
eval (Mul x y)  = (eval x) * (eval y)

toMayBe :: (a -> b) -> Maybe a -> Maybe b
toMayBe _ Nothing   = Nothing
toMayBe f (Just x)  = Just (f x)

evalMayBe :: Maybe ExprT -> Maybe Integer
evalMayBe = toMayBe eval

evalStr :: String -> Maybe Integer
evalStr i = evalMayBe $ parseExp Lit Add Mul i

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>) 0
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit = Mod7 . flip mod 7
    add (Mod7 x) (Mod7 y) = lit (x + y)
    mul (Mod7 x) (Mod7 y) = lit (x * y)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

class HasVars a where
    var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | VVar String
    deriving (Show, Eq)

instance Expr VarExprT where
    lit = VLit
    add = VAdd
    mul = VMul

instance HasVars VarExprT where
    var = VVar

type VarMapper = (M.Map String Integer -> Maybe Integer)

varMapAdd :: VarMapper -> VarMapper -> M.Map String Integer -> Maybe Integer
varMapAdd x y m
    | otherwise = error "not implemented"
    where 
        xVal = x m
        xVal :: Maybe Integer
        yVal = y m
        yVal :: Maybe Integer

instance HasVars VarMapper where
    var key = M.lookup key

{-
instance Expr VarMapper where
    lit x   = const (Just x)
    add x y = (\m =
        | otherwise = error "not implemented"
    mul x y = const (Just (x * y))
-}