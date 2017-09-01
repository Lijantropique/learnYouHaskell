import System.Environment

main = do
    (fileName1:fileName2:_) <- getArgs
    copyFile fileName1 fileName2


copyFile :: FilePath -> FilePath -> IO()
copyFile source dest = do
    contents <- readFile source
    writeFile dest contents
