import Data.Char (isDigit)
import Data.List

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ sum $ mapToFirstLast $ mapToDigits $ lines input
  print $ sum $ mapToFirstLast $ intercalateLists $ listStringToNumbers $ filterNonDigits $ lines input

-- Part 1:

mapToDigits :: [String] -> [String]
mapToDigits = map (filter isDigit)


mapToFirstLast :: [String] -> [Int]
mapToFirstLast = map (\w -> read (take 1 w ++ take 1 (reverse w))::Int)


-- Part 2:

filterNonDigits :: [String] -> [[String]]
filterNonDigits = map (filter (\sub -> length sub == 1 && all isDigit sub || sub `elem` ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]) . nonEmptySubstrings)

stringToNumber :: String -> String
stringToNumber s
              | s == "one" = "1"
              | s == "two" = "2"
              | s == "three" = "3"
              | s == "four" = "4"
              | s == "five" = "5"
              | s == "six" = "6"
              | s == "seven" = "7"
              | s == "eight" = "8"
              | s == "nine" = "9"
              | otherwise = "0"

listStringToNumbers :: [[String]] -> [[String]]
listStringToNumbers = map (map (\s -> if all isDigit s then s else stringToNumber s))

intercalateLists :: [[String]] -> [String]
intercalateLists list = mapToDigits (map unwords list)

nonEmptySubstrings :: [a] -> [[a]]
nonEmptySubstrings = concatMap (tail . inits) . tails
-- https://stackoverflow.com/questions/19525022/all-subsequences-from-a-single-string