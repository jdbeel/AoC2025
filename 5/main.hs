import System.Environment (getArgs)
import Data.List (foldl', foldr, sortBy)

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
    "" -> []
    s' -> w : split p s''
        where (w, s'') = break p s'

stringToRange :: String -> (Integer, Integer)
stringToRange s = (read first :: Integer, read second :: Integer)
    where [first, second] = split (=='-') s

checkId :: ([(Integer, Integer)], Integer) -> Integer -> Integer
checkId (ranges, prev) id
    | any (\(l, u) -> id >= l && id <= u) ranges = prev + 1
    | otherwise = prev + 0

checkRange :: ([(Integer, Integer)], (Integer, Integer)) -> Bool
checkRange (ranges, (l, u)) = any (\(rl, ru) -> l >= rl && u <= ru) ranges

sortRange :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Ordering
sortRange (ll, lu) (rl, ru)
    | ll < rl = LT
    | ll > rl = GT
    | otherwise = compare lu ru

mergeRanges :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeRanges = foldr f []
    where f new@(xl, xu) acc@((yl, yu):ys) =
            if yl <= xu
            then (xl, max yu xu):ys
            else new:acc
          f x acc = x:acc

testRanges :: [(Integer, Integer)]
testRanges = sortBy sortRange [(3,5), (10,14), (16,20), (12,18)]
testIds = [1, 5, 8, 11, 17, 32]


main :: IO()
main = do
    args <- getArgs
    content <- readFile (args !! 0)

    let lines' = lines content
    let ranges = sortBy sortRange $ map stringToRange $ takeWhile (/= "") lines'
    let ids = map read $ drop (length ranges + 1) lines' :: [Integer]
    let resultPartOne = foldl' (curry checkId ranges) 0 ids
    print resultPartOne
    let resultPartTwo = sum $ map (\(l, u) -> (u - l) + 1) $ mergeRanges ranges
    print resultPartTwo
