module LogAnalysis where

import Debug.Trace

import Log

-- |parseMessageWords parses list of words
parseMessageWords :: [String] -> LogMessage
parseMessageWords (mType:(tsString:msg))
    | mType == "I" = LogMessage Info ts (unwords msg)
    | mType == "W" = LogMessage Warning ts (unwords msg)
    where
        ts = read tsString :: Int
parseMessageWords ("E":(errString:(tsString:msg))) = LogMessage (Error err) ts (unwords msg)
    where
        ts = read tsString :: Int
        err = read errString :: Int
parseMessageWords x = Unknown (unwords x)

-- |parseMessage parses an individual line from a log file
parseMessage :: String -> LogMessage
parseMessage = parseMessageWords . words

-- |parse parses an entire log file at once and returns its contents
parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

-- |insert inserts a new LogMessage into an existing MessageTree,
-- producing a new MessageTree.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) x = x
insert x Leaf = Node Leaf x Leaf
insert x (Node _ (Unknown _) _) = Node Leaf x Leaf
insert x y
    | xTime > yTime = Node y x Leaf
    | otherwise     = Node Leaf x y
    where 
        (LogMessage _ xTime _) = x
        (Node _ (LogMessage _ yTime _) _) = y

insertRev x y = insert y x

build :: [LogMessage] -> MessageTree
build = foldl insertRev Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error lvl) ts msg)
    | lvl >= 50 = True
    | otherwise = False
isRelevant _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map getMessage) . inOrder . build . (filter isRelevant)