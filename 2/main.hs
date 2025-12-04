import Data.Fixed
import Debug.Trace
import System.Environment
import Data.Maybe
import Text.Read

isRepeatedA :: Integer -> Bool
isRepeatedA x = left == right
    where  digits = getDigits x
           (left, right) = splitAt (length digits `div` 2) digits

-- Seems silly to pass around numDigits, and maybe it is, but it hopefully means we only have
-- to compute it once per number which is better than more than once :)
isRepeatedB :: Integer -> Bool
isRepeatedB x = isRepeatedBInner digits (length digits) Nothing
    where digits = getDigits x

isRepeatedBInner :: [Integer] -> Int -> Maybe Int -> Bool
isRepeatedBInner [] _ _ = False
isRepeatedBInner digits numDigits (Just i)
        -- short circuit if we're looking at substrings too long to repeat
        | i > (numDigits `div` 2) = False
        -- cycle the current substring and pull from it until it has repeated numDigits `div` i times and check
        -- if it matches
        | take (i * (numDigits `div` i)) (cycle $ take i digits) == digits = True
        -- Otherwise recurse, grabbing a larger substring to check
        | otherwise = isRepeatedBInner digits numDigits $ Just (i + 1)
isRepeatedBInner digits numDigits Nothing = isRepeatedBInner digits numDigits (Just 1)


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
    let invalidIdsA = map (\x -> filter isRepeatedA [fst x..snd x]) processed
    let invalidIdsB = map (\x -> filter isRepeatedB [fst x..snd x]) processed
    -- print invalidIdsA -- Too many things to reasonably look at
    -- print invalidIdsB -- Too many things to reasonably look at
    print $ (sum . concat) invalidIdsA
    print $ (sum . concat) invalidIdsB

