module Main where

-- import StringBuffer
import Editor
import JoinList
import Buffer

buffer :: ScoreStringBuffer
buffer = fromString $ unlines
  [ "This buffer is for notes you don't want to save, and for"
  , "evaluation of steam valve coefficients."
  , "To load a different file, type the character L followed"
  , "by the name of the file."
  ]

main = runEditor editor $ buffer

{-
main = runEditor editor $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
-}