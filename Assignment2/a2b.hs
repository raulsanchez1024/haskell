-- Raul Sanchez
-- Assignment2

module A2b where

-- Helper Methods (from in-class examples)
split :: [a] -> ([a], [a])
split xs = splitAt (div (length xs) 2) xs

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort xs =
  let (first, second) = split xs
    in
      merge (mergeSort first) (mergeSort second)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge (x:xs) [] = (x:xs)
merge [] (y:ys) = (y:ys)
merge (x:xs) (y:ys) = if x < y
                        then x : (merge xs (y:ys))
                        else y : (merge (x:xs) ys)

-- 1. removeAllExcept
-- Consume varaible and a list
-- Produce a list containing just that variable
removeAllExcept :: (Eq a) => a -> [a] -> [a]
removeAllExcept a [] = []
removeAllExcept a (x:xs) | a == x =
                                    x : removeAllExcept a xs
                                    | otherwise = removeAllExcept a xs

-- 2. removeAll
-- Consume a single variable x and a list (y:ys)
-- Produce a list that does not include x
removeAll :: (Eq a) => a -> [a] -> [a]
removeAll a [] = []
removeAll a (x:xs) | a == x =
                              removeAll a xs
                              | otherwise = x : removeAll a xs

-- 3. substitute
-- Consume two variables a b and a list
-- Produce a list that a was replaced with b
substitute :: (Eq a) => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute a b (x:xs) | a == x =
                                b : substitute a b xs
                                | otherwise = x : substitute a b xs

-- 4. mergeSorted3
-- Consume three lists
-- Produce one sorted list constructed from the three consumed lists
mergeSorted3 :: (Ord a) => [a] -> [a] -> [a] -> [a]
mergeSorted3 [] [] [] = []
mergeSorted3 (x:xs) [] [] = x : mergeSorted3 xs [] []
mergeSorted3 [] (y:ys) [] = y : mergeSorted3 [] ys []
mergeSorted3 [] [] (z:zs) = z : mergeSorted3 [] [] zs
mergeSorted3 (x:xs) (y:ys) [] =
                                if x < y
                                  then x : mergeSorted3 xs (y:ys) []
                                  else y : mergeSorted3 ys (x:xs) []
mergeSorted3 [] (y:ys) (z:zs) =
                                if y < z
                                  then y : mergeSorted3 [] ys (z:zs)
                                  else z : mergeSorted3 [] zs (y:ys)
mergeSorted3 (x:xs) [] (z:zs) = if x < z
                                  then x : mergeSorted3 xs [] (z:zs)
                                  else z : mergeSorted3 zs [] (x:xs)
mergeSorted3 (x:xs) (y:ys) (z:zs) = mergeSort(x : y : z : mergeSorted3 xs ys zs)

-- 5. TriTree
-- Construct a TriTree
instance (Eq a) => Eq (TriTree a) where
  EmptyNode           == EmptyNode = True
  TriNode a la ma ra  == TriNode b lb mb rb = (a == b) &&
                                              (la == lb) &&
                                              (ma == mb) &&
                                              (ra == rb)
  _                   == _ = False

data TriTree a =
                EmptyNode | TriNode a (TriTree a) (TriTree a) (TriTree a)
                  deriving (Show)

-- 5.1 nodeValue
-- Consume a trinary tree
-- Produces the value of the given node
nodeValue :: TriTree a -> a
nodeValue EmptyNode = error "node is empty"
nodeValue (TriNode a la ma ra) = a

-- 5.2 leftChild
-- Consume a trinary tree
-- Produces/returns the left child
leftChild :: TriTree a -> TriTree a
leftChild EmptyNode = error "tree is an empty node"
leftChild (TriNode a la ma ra) = la

-- 5.3 middleChild
-- Consume a trinary tree
-- Produces/returns the middle child
middleChild :: TriTree a -> TriTree a
middleChild EmptyNode = error "tree is an empty node"
middleChild (TriNode a la ma ra) = ma

-- 5.4 rightChild
-- Consume a trinary tree
-- Produces/returns the right child
rightChild :: TriTree a -> TriTree a
rightChild EmptyNode = error "tree is an empty node"
rightChild (TriNode a la ma ra) = ra

-- 5.5 inTree
-- Consume a given variable and a trinary tree
-- Produces/returns True or False if the given element is in a trinary tree
inTree :: (Eq a) => a -> TriTree a -> Bool
inTree _ EmptyNode = False
inTree x (TriNode a la ma ra) | x == a =
                                        True
                                        | otherwise = inTree x la || inTree x ma || inTree x ra

-- 5.6 leafList
-- Consumes a trinary tree
-- Produces a list of all the values in the leaves of the tree
leafList :: TriTree a -> [a]
leafList EmptyNode = []
leafList (TriNode a EmptyNode EmptyNode EmptyNode) = [a]
leafList (TriNode _ la ma ra) =
                                leafList la ++ leafList ma ++ leafList ra

-- 5.7 inOrderMap
-- Consumes a function that takes type a and returns type b, pass in a TriTree with type a
-- Produces TriTree of type b
inOrderMap :: (a -> b) -> TriTree a -> TriTree b
inOrderMap _ EmptyNode = EmptyNode
inOrderMap x (TriNode a la ma ra) =
                                    TriNode (x a) (inOrderMap x la) (inOrderMap x ma) (inOrderMap x ra)

-- 5.8 preOrderFold
-- Consumes a function as the first argument, an accumulator value, a TriTree
-- Produces a pre-order walk of the tree, applying the function to each value
--          and then using the result of that function in the next call of the folding in the tree
preOrderFold :: (Eq a) => (b -> a -> b) -> b -> TriTree a -> b
preOrderFold _ y EmptyNode = y
preOrderFold x y (TriNode a la ma ra)
  | (la == EmptyNode && ma == EmptyNode && ra == EmptyNode)
                                    = x y a
                                    | otherwise = preOrderFold x (preOrderFold x (x (preOrderFold x y la) a) ma) ra
