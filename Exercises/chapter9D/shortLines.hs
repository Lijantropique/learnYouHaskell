main = do
    contents <- getContents
    putStr $ shortLine contents

shortLine :: String -> String
shortLine str =
    let
        allLines = lines str
        shortLine = filter (\x-> length x <10) allLines
        in unlines shortLine
