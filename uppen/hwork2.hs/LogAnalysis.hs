{-# OPTIONS_GHC -Wall #-}
-- ====|
--    Homework 2 from 'Introduction to Haskell (Spring 2013)'
--    September 2017
-- |====

module LogAnalysis where

import Log

-- ===| Exercise 1 |=== --
-- Parse an error message from the log file
parseMessage :: String -> LogMessage
parseMessage (t:' ':xs)
    |t=='I'     = LogMessage Info n' s
    |t=='W'     = LogMessage Warning n' s
    |t=='E'     = LogMessage (Error n') s' msg'
    where
        (n:msg) = words xs
        s = if null msg then "" else unwords msg
        n' = (read n:: Int)
        s' = (read (head msg):: Int)
        msg' = unwords $ tail msg
parseMessage e  = Unknown e

-- Parse a complete log file
parse :: String -> [LogMessage]
parse str = let
    logList = lines str
    in map parseMessage logList

-- ===| Exercise 2 |=== --
-- Insert a LogMessage into an existing MessageTree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msnTree = msnTree
insert newMessage Leaf = Node Leaf newMessage Leaf
insert newMessage (Node left logMessage right)
    |ts newMessage < ts logMessage  = Node (insert newMessage left) logMessage right
    |ts newMessage > ts logMessage  = Node left logMessage (insert newMessage right)
    where
        ts (LogMessage _ timeStamp _) = timeStamp
insert _ msnTree = msnTree


-- ===| Exercise 3 |=== --
-- Build a MessageTree from a list of LogMessage
build :: [LogMessage] -> MessageTree
build logList = foldr insert Leaf logList


-- ===| Exercise 4 |=== --
-- List of sorted messages in a MessageTree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf= []
inOrder (Node left logMessage right) = inOrder left ++ [logMessage] ++ inOrder right


-- ===| Exercise 5 |=== --
-- Return a sorted list of LogMessage with Erros of severity >= 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logList = map getMessage $ filter severeError sortedList
    where
        sortedList = inOrder (build logList)
        severeError (LogMessage (Error n) _ _)
            |n>=50 = True
            |otherwise = False
        severeError _ = False
        getMessage (LogMessage _ _ msn) = msn
