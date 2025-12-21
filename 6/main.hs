import System.Environment (getArgs)
import Data.List (foldl', foldr)
-- import Data.Sequence (Seq((:|>)), fromList)
import Data.Foldable (toList)

getOperandsAt :: [[Integer]] -> Int -> [Integer]
getOperandsAt operands i = map (!! i) operands

stringsToOperands :: [[String]] -> [[Integer]]
stringsToOperands = map (map (read :: String->Integer))

stringsToOperands' :: [String] -> [Integer]
stringsToOperands' = map (read :: String->Integer)

applyOperator :: String -> [Integer] -> Integer
applyOperator "*" operands = foldl' (*) 1 operands
applyOperator "+" operands = foldl' (+) 0 operands

getColumnAt :: [String] -> Int -> String
getColumnAt lines' i = filter (/= ' ') $ map (!! i) lines'

toColumns :: [String] -> [String]
toColumns xs'@(x:xs) = map (getColumnAt xs') [len,len-1..0]
    where len = length x - 1

splitColumns :: [String] -> [[String]]
splitColumns [] = []
splitColumns cols = col : splitColumns (drop (length col + 1) cols)
    where col = takeWhile (/= "") cols

extractOperators :: [String] -> ([String], String)
extractOperators col = (nums, [operator])
    where nums = map (takeWhile (\x -> x /= '+' && x /= '*')) col
          operator = last $ last col

main :: IO()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let lines' = lines content
    let wordsLines = map words lines'
    let operands = stringsToOperands $ init wordsLines
    let operatorStrings = last wordsLines

    let resultPartOne = map (\i -> applyOperator (operatorStrings !! i) (getOperandsAt operands i)) [0..length operatorStrings - 1]
    print $ sum resultPartOne

    let splitCols = map extractOperators $ (splitColumns . toColumns) lines'
    let resultPartTwo = map (\(x, y) -> applyOperator y (stringsToOperands' x)) splitCols
    print $ sum resultPartTwo
