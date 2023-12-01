import Data.Char (isNumber, isDigit)
import System.Win32 (COORD(yPos), wAIT_ABANDONED)
main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ sum $ mapToFirstLast $ mapToDigits $ lines input


mapToDigits :: [String] -> [String]
mapToDigits = map (filter isDigit)


mapToFirstLast :: [String] -> [Int]
mapToFirstLast = map (\w -> read (take 1 w ++ take 1 (reverse w))::Int)
-- use first and reverse
