main = interact palindrome

palindrome :: String -> String
palindrome str = unlines $ map isPalindrome $ lines str
    where
        isPalindrome x = if reverse x == x
            then "palindrome"
            else "not palindrome"
