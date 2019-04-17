{-# LANGUAGE ViewPatterns #-}
module Homework2.LogAnalysis where

import Data.List

import Homework2.Log

parseTimestamp :: String -> (TimeStamp, String)
parseTimestamp = error "unimplemented"

-- |parseMessageWords parses list of words
parseMessageWords :: [String] -> LogMessage
parseMessageWords (mType:(tsString:msg))
    | mType == "I" = LogMessage Info ts (unwords msg)
    | mType == "W" = LogMessage Warning ts (unwords msg)
    where
        ts = read tsString :: Int
parseMessageWords ("E":(tsString:(errString:msg))) = LogMessage (Error err) ts (unwords msg)
    where
        ts = read tsString :: Int
        err = read errString :: Int
parseMessageWords x = Unknown (unwords x)

-- |parseMessage parses an individual line from a log file
parseMessage :: String -> LogMessage
parseMessage = parseMessageWords . words
{-
parseMessage (stripPrefix "I " -> Just rest) = LogMessage Info time message
    where (time, message) = parseTimestamp rest
parseMessage s = Unknown s
-}

-- |parse parses an entire log file at once and returns its contents
parse :: String -> [LogMessage]
parse s = error "not implemented"