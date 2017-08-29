import System.IO

main = do
    item <- getLine
    if null item
        then return ()
        else do
            appendFile "todoList" $ item ++  "\n"
            main
