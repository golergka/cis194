module Homework5.Calc where

import Homework5.ExprT
import Homework5.Parser

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