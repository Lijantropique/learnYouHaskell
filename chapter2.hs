-- Baby's first functions

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 2
conanO'Brien = "It's a-me, Conan O'Brien!"

-- An intro to lists & Texas ranges
lostNumbers = [4,8,15,16,23,42]
seqTest =  [1,2,3,4,5,6]
appendList list1 list2 = list1 ++ list2
prependList val list = val:list
searchList list idx = list !! idx

a = [1..9]
a' = [9,8..1]
b = [[1,2,3,4],[5,6,7],[8,9]]

listOfList list = if not (null list)
                    then [head list]:listOfList (tail list)
                    else []
revLoL list = reverse (listOfList list)

-- countItem list item = if elem item list
--                       then 1 + countItem (dropWhile (/=item) list) item
--                       else 0

hasPunctuation string = if (head string `elem` ['a'..'z']++['A'..'Z'])
                            then  if null (tail string)
                                  then False
                                  else hasPunctuation (tail string)
                            else True

-- I'm a list comprehension
doubleRange upper = [doubleMe x|x<-[1..upper]]
doubleRangeOdd upper = [doubleMe x|x<-[1..upper],odd x]
doubleRangeNoTeens upper = [doubleMe x|x<-[1..upper],doubleMe x <10 || doubleMe x>20]
doubleRangeno6_8 upper = [doubleMe x|x<-[1..upper],doubleMe x /=6, doubleMe x/=8]

describeRange upper = [if odd x then "ODD" else "EVEN"|x<-[1..upper]]
