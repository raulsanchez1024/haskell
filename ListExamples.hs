-- multElemnts
-- Consume a list of numbers
-- Produce a list which consists of each element *n
multElements theList n = if (theList == [])
                            then []
                            else ((head theList) * n) : (multElements                                    (tail theList) n)

-- doubleList
-- Consume a list of numbers
-- Produce a list of lists for example
--    [1,2,3] -> [(1,2), (2,4), (3,6)]
doubleList theList = if (theList == [])
                        then []
                        else ((head theList),
                              ((head theList * 2))) : doubleList (tail theList)

-- doubleElemnts
-- Consume a list of numbers
-- Produce a list which consists of each element *2
doubleElements theList = if (theList == [])
                            then []
                            else ((head theList) * 2) : doubleElements                                    (tail theList)

-- addElements
-- Consume a list of numebers
-- Produce a number which is the sum of the list addElements
addElements theList = if (theList == [])
                        then 0
                        else (head theList) + addElements (tail theList)
