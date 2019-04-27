-- Raul Sanchez
-- Assignment1

module A1c where

-- helperRev
-- Consume a list
-- Produce a reversed list
helperRev :: [a] -> [a]
helperRev [] = []
helperRev (x:xs) = helperRev xs ++ [x]

-- helperZip
-- Consume two lists
-- Produce zipped list of both lists
helperZip [] [] = []
helperZip (x:xs) (y:ys) = [(y,x)] ++ helperZip xs ys

-- sDotProduct
-- Consume an integer called mult and two tuples
-- Produce the sDotProduct of the two tuples
sDotProduct mult t1 t2 = mult * ((fst t1 * fst t2) + (snd t1 * snd t2))

-- distance
-- Consume two tuples
-- Produce the distance
distance (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

-- tripleDistance
-- Consume two tuples of three elements
-- Produce the distance
tripleDistance (x1,y1,z1) (x2,y2,z2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)

-- findMin
-- Consume a list
-- Produce the minimum element in the list
findMin xs = head [x | x <- xs, all (>= x) xs]

-- tupleDotProduct
-- Consume two lists
-- Produce the dot product
tupleDotProduct [] [] = 0
tupleDotProduct (x:xs) (y:ys) = x * y + tupleDotProduct xs ys

-- revZip2Lists
-- Consume two lists
-- Produce a list with a pair from each list consecutively
revZip2Lists [] [] = []
revZip2Lists a b = helperRev (helperZip a b)

-- everyThirds
-- Consume a list
-- Produce a new list only consisting of every third element in it
everyThird xs = [x+2 | (1,x) <- zip (cycle [1..3]) xs]
