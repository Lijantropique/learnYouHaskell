-- The most wanted letter
-- htpps://py.checkio.org/mission/most-wanted-letter/


lowerL letter = if letter `elem` ['A'..'Z']
                then head [snd item | item <- zip ['A'..'Z']['a'..'z'], letter == fst item]
                else letter
lowerS string = [lowerL x | x <- string, x `elem` ['a'..'z']++['A'..'Z']]
count string letter = sum[1 | x <- string, x==letter]

checkio string = snd (maximum[(count string x,x)| x <- lowerS string])
