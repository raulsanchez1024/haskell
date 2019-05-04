-- Raul Sanchez
-- Assignment3

module A3b where
import Data.List
import Data.Char

-- ========== PART I ========== --

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
longestString xs =
  foldl (\x acc -> if length x > length acc then x else acc) "" xs

-- 3.
-- longestString'
-- Consume a list of strings
-- Produce/return the longest string. NOTE: if two words have the save
-- length, then return the last longest string
longestString' :: [String] -> String
longestString' xs =
  foldl (\x acc -> if length acc <= length x then x else acc) "" xs

-- 4.1
-- longestStringHelper
-- Consume a function that evaluates a Bool
-- Produce/return longest String in the list
longestStringHelper :: (Int -> Int -> Bool) -> [String] -> String
longestStringHelper _ [] = []
longestStringHelper a (x:xs) =
  foldl (\b acc -> if a (length acc) (length b) then acc else b) x xs


-- 4.2
-- longestString3
-- Consume a list of Strings
-- Produce/return the longest string
longestString3 :: [String] -> String
longestString3 = longestStringHelper (>)

-- 4.3
-- longestString4
-- Consume a list of Strings
-- Produce/return the longest string
longestString4 :: [String] -> String
longestString4 = longestStringHelper (<=)

-- 5.
-- longestLowercase
-- Consume a list of Strings
-- Produce/return longest lowercase string
longestLowercase :: [String] -> String
longestLowercase = longestLowercase . onlyLowercase

-- 6.
-- revStringRev
-- Consume a String
-- Produce a reversed and converted to lowercase string
revStringRev :: String -> String
revStringRev = reverse . map toLower

-- 7.
-- firstAnswer
-- Consume <a> variable and Maybe <b>
-- Produce/return Maybe <b>
firstAnswer :: (Eq b) => ( a -> Maybe b ) -> [a] -> Maybe b
firstAnswer _ [] = Nothing
firstAnswer a (x:xs) | a x /= Nothing = a x | otherwise = firstAnswer a xs

-- 8.
-- allAnswers
-- Consume <a> variable and Maybe a list of <b>
-- Produce/return Maybe a list of <b>
allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
allAnswers _ [] = Just []

-- ========== PART II ========== --

data Pattern = WildcardPat | VariablePat (String) | UnitPat | ConstantPat (Int) | ConstructorPat (String, Pattern) | TuplePat ([Pattern]) deriving (Eq, Show)
data Value = Constant (Int) | Unit | Constructor (String, Value) | Tuple [Value] deriving (Eq, Show)

-- 1.
-- checkPat
-- Consume a Pattern
-- Produce a boolean
-- checkPat :: Pattern -> Bool

-- 2.
-- match
-- Consume a Tuple of Value and Patter
-- Produce/return maybe list of Tuples of String and Value
-- match  (Value, Pattern) -> Maybe [(String, Value)]

-- 3.
-- firstMatch
-- Consume a Value
-- Produce/return Maybe a list of tuples of tupe String and Value
-- firstMatch :: Value -> [Pattern] -> Maybe [(String, Value)]
