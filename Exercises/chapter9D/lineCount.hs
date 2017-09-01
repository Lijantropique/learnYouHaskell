import System.Environment
import System.IO
import System.Directory

main :: IO ()
main = do
    (fileName:_) <- getArgs
    fileExists <- doesFileExist fileName
    if fileExists --and fileName/="")
        then do
            contents <- readFile fileName
            putStrLn $ "The file '" ++ fileName ++ "' has " ++ show (length $ lines contents) ++ " lines!"
        else do
            putStrLn $"The file '" ++ fileName ++ "' doesn't exits"
