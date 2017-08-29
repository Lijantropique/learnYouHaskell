import System.IO
import Data.Char

main = do
    contents <- readFile "girlfriend"
    writeFile "girlfriendCAPS" (map toUpper contents)
    putStr contents
