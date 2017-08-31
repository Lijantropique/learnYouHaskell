import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.DeepSeq

main = do
    (command:args) <- getArgs
    (dispatch command) args

dispatch :: String -> ([String] -> IO())
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove

add :: [String] -> IO()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO()
view [fileName] = do
    contents <- readFile fileName
    putStr $ coupled contents
    where
        coupled xs = unlines $ zipWith (\idx str-> show idx ++ "-" ++ str) [1..] (lines xs)

remove :: [String] -> IO()
remove [fileName, idx] = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let
        lst = lines contents
        idx' = read idx
        tempContent = unlines $ delete (lst!!(idx'-1)) lst
    tempContent `deepseq` hClose handle --force evaluation of 'tempContent' and return IO action 'hClose handle'
    writeFile fileName tempContent

-- remove :: [String] -> IO()
-- remove [fileName, idx] = do
--     handle <- openFile fileName ReadMode
--     (tempName, tempHandle) <- openTempFile "." "temp"
--     contents <- hGetContents handle
--     let
--         lst = lines contents
--         idx' = read idx
--         tempContent = unlines $ delete (lst!!(idx'-1)) lst
--     hPutStr tempHandle tempContent
--     hClose handle
--     hClose tempHandle
--     removeFile fileName
--     renameFile tempName fileName
