import System.Environment
import Data.List (foldl', foldr)
import Debug.Trace
import Data.Ord
import Data.List

-- | Returns the index of the first occurrence of the maximum value in a list.
indexMax :: Ord a => [a] -> (Int, a)
indexMax (x:xs) =  foldl' maxByVal (0, x) (zip [1..] xs)
  where
    maxByVal (idxA, valA) (idxB, valB)
      | valB > valA = (idxB, valB)
      | otherwise   = (idxA, valA)

processLineA :: String -> Integer
processLineA line = d1 * 10 + d2
  where digits = map (read . (:"")) line :: [Integer]
        (i1, d1) = indexMax $ init digits
        (_, d2) = indexMax $ drop (i1 + 1) digits

fromDigits :: [Integer] -> Integer
fromDigits = foldl' addDigit 0
  where addDigit num d = 10 * num + d

processLineB :: String -> Integer
processLineB line = voltage
  where digits = map (read . (:"")) line :: [Integer]
        voltageDigits = processLineB' 0 digits
        voltage = fromDigits voltageDigits

processLineB' :: Int -> [Integer] -> [Integer]
processLineB' currentDigit digits
  | null digits = [] -- Null case, not really used
  | currentDigit == 11 = [snd (indexMax digits)] -- Just take the index max of what's left
  | 11 - currentDigit >= length digits = digits -- If there are only as many digits left as we need, take them all
  | otherwise = d : processLineB' (currentDigit + 1) (drop (i + 1) digits)
    where (i, d) = indexMax subdigits
          subdigits = take (length digits - (11 - currentDigit)) digits

main :: IO()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  -- print $ (map processLineB . lines) content
  print $ (sum . map processLineB . lines) content

  -- print $ processLineB "818181911112111"

