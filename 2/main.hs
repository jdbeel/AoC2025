import Data.Fixed
import Debug.Trace
import System.Environment
import Data.Maybe
import Text.Read

isPalindrome :: Integer -> Bool
isPalindrome x = left == right
    where  digits = getDigits x
           (left, right) = splitAt (length digits `div` 2) digits


split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
    "" -> []
    s' -> w : split p s''
        where (w, s'') = break p s'

processInput :: String -> [(Integer, Integer)]
processInput = i . h . g . f where
    f = split (==',')
    g = map $ split (=='-')
    h = map $ mapMaybe readMaybe :: [[String]] -> [[Integer]]
    i = map toTuple

toTuple :: [Integer] -> (Integer, Integer)
toTuple [x, y] = (x, y)

getDigits :: Integer -> [Integer]
getDigits 0 = []
getDigits n = let (q, r) = n `divMod` 10 in getDigits q ++ [r]

main :: IO()
main = do
    args <- getArgs
    content <- readFile (args !! 0)

    let processed = processInput content
    let invalidIds = map (\x -> filter isPalindrome [fst x..snd x]) processed
    -- print invalidIds -- Too many things to reasonably look at
    print $ (sum . concat) invalidIds
