import Data.List
import Data.Char

-- onlyLowercase
-- Consume
-- Produce
onlyLowercase :: [String] -> [String]
onlyLowercase = filter (\x -> ("" /= x) && (isLower $ head x));

-- foldl (\x xs -> if length x < length xs then xs else x)
-- foldl (\accmax xs -> if length accmax < length xs then xs else accmax) [] ["one", "two", "three", "four", "five"]

-- longestString
-- Consume
-- Produce
longestString :: [String] -> String
longestString xs = foldl (\x acc -> if length x > length acc then x else acc) "" xs

-- longestString'
-- Consume
-- Produce
longestString' :: [String] -> String
longestString' xs = foldl (\x acc -> if length x > length acc then x else acc) "" xs
