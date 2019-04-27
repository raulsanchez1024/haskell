factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

sumOfList :: (Num a) => [a] -> a
sumOfList [] = 0
sumOfList (x:xs) = x + sumOfList xs

findMin :: (Ord a) => [a] -> a
findMin [] = error "Find min on empty list"
findMin (x:y:xs) =
          let m = findMin xs
            in
              if (x < m)
                    then x
                    else m

quicksort :: (Ord a) => [a] -> a
quicksort [] = []
quicksort (x:xs) =
            let smaller = [a | a <- xs, a <= x]
                larger = [a | a <- xs, a > x]
                in
                  quicksort smaller ++ [x] ++ quicksort larger
