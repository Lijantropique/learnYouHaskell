import System.IO

main = do
    contents <- readFile "girlfriend"
    putStr contents
