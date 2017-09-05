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
    |t=='I'     = LogMessage Info n' (s ++ " " ++ msg')
    |t=='W'     = LogMessage Warning n' (s ++ " " ++ msg')
    |t=='E'     = LogMessage (Error n') s' msg'
    where
        (n:s:msg) = words xs
        n' = (read n:: Int)
        s' = (read s:: Int)
        msg' = unwords msg
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



-- build :: [LogMessage] -> MessageTree
-- build logList = foldl insert Leaf logList
