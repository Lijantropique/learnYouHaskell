import Data.List

solveRPN :: String -> Float
solveRPN expression = head $ foldl foldingFunction [] (words expression)
    where
        foldingFunction (x:y:ls) "+" = (x + y):ls
        foldingFunction (x:y:ls) "-" = (y - x):ls
        foldingFunction (x:y:ls) "*" = (x * y):ls
        foldingFunction (x:y:ls) "/" = (y / x):ls
        foldingFunction (x:y:ls) "^" = (y ** x):ls
        foldingFunction (x:ls) "ln" = log x:ls
        foldingFunction ls "sum" = [sum ls]
        foldingFunction ls  numberString = read numberString:ls
