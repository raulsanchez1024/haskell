import Data.Maybe

-- data MyList a = Empty | Node a (MyList a)
--       deriving (Eq, Ord, Show)

-- Node 5 (Node 3 Empty Empty) Empty
data Tree a = Empty | Node a (Tree a) (Tree a)
      deriving (Eq, Ord, Show)

sumTree Empty = 0
sumTree (Node a b c) = a + sumTree b + sumTree c

multTree Empty = 1
multTree (Node a b c) = a * multTree b * multTree c

doubleTree Empty = Empty
doubleTree (Node a b c) = (Node (2*a) (doubleTree b) (doubleTree c))

addTwoTree Empty = Empty
addTwoTree (Node a b c) = (Node (a+2) (addTwoTree b) (addTwoTree c))

myTake m xs = case (m, xs) of
                    (0, xs) -> []
                    (m, []) -> []
                    (m, x:xs) -> x : myTake (m-1) xs

undoMaybe :: Maybe a -> a
undoMaybe Nothing = error "Undoing Nothing"
undoMaybe (Just a) = a

findMin :: (Ord a) => [a] -> Maybe a
findMin [] = Nothing
findMin[a] = Just a
findMin (x:xs) =
          let m = findMin xs
            in if (Just x < m)
                    then Just x
                    else m

-- anon func
-- map (\x -> x * x) [1,2,3]
