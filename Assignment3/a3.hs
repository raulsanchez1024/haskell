-- Raul Sanchez
-- Assignment3

import Data.List
import Data.Char

-- 1.
-- onlyLowercase
-- Consume a list of strings
-- Produce/return a list of strings that start with a lower case letter
onlyLowercase :: [String] -> [String]
onlyLowercase = filter (\x -> ("" /= x) && (isLower $ head x));

-- 2.
-- longestString
-- Consume a list of strings
-- Produce/return the longest string. NOTE: if two words have the save
-- length, then return the first longest string
longestString :: [String] -> String
longestString xs = foldl (\x acc -> if length acc > length x then acc else x) "" xs

-- 3.
-- longestString'
-- Consume a list of strings
-- Produce/return the longest string. NOTE: if two words have the save
-- length, then return the last longest string
longestString' :: [String] -> String
longestString' xs = foldl (\x acc -> if length x > length acc then x else acc) "" xs

-- 4.1
-- longestString3
-- Consume
-- Produce
