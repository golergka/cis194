{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import Control.Applicative
import Data.Char

import AParser

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
-- zeroOrMore p = ((:) <$> p <*> (zeroOrMore p)) <|> pure []
zeroOrMore = many
    

oneOrMore :: Parser a -> Parser [a]
oneOrMore = some

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> (zeroOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser Atom
parseAtom = (N <$> posInt) <|> (I <$> ident)

parseSExprA :: Parser SExpr
parseSExprA = A <$> parseAtom

parseSExprComb :: Parser SExpr
parseSExprComb = char '(' *> (Comb <$> oneOrMore parseSExpr) <* char ')'

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseSExprA <|> parseSExprComb) <* spaces