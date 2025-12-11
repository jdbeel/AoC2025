import System.Environment
import Data.List

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

main :: IO()
main = do
    args <- getArgs
    content <- readFile (args !! 0)

    let lines' = lines content
    let ranges = map stringToRange $ takeWhile (/= "") lines'
    let ids = map read $ drop (length ranges + 1) lines' :: [Integer]
    let resultPartOne = foldl' (curry checkId ranges) 0 ids
    print resultPartOne