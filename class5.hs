foo f a = map f (take 3 a)
-- foo (2*) [1,2,3]
-- foo (\x -> 2*x) [1,2,3,4] anon func

nTimes f n x =
  if n == 0
    then x
    else f (nTimes f (n-1) x)

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
