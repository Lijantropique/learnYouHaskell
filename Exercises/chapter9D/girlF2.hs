import System.IO

main = do
    withFile "girlfriend" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)
