import System.IO
import System.Directory
import Data.List

main = do
    contents <- readFile "todoList"
    putStrLn "These are your current TO-DO items:"
    putStr $ enumerate contents
    putStrLn "Which one do you want to delete:"
    numberString <- getLine
    writeFile "todoList" $ deleteLine contents numberString


enumerate :: String -> String
enumerate xs =
    let
        coupled = zipWith (\idx str-> show idx ++ "-" ++ str) [1..] (lines xs)
        in unlines coupled

deleteLine :: String -> String -> String
deleteLine xs idx =
    let
        lst = lines xs
        idx' = read idx
        in unlines $ delete (lst!!(idx'-1)) lst
