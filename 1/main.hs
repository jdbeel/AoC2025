import System.Environment ( getArgs )
import Data.List ( mapAccumL )

instructionToNumber :: ([Char], [Char]) -> Integer
instructionToNumber ("L", x) = - (read x :: Integer)
instructionToNumber ("R", x) = read x :: Integer

splitInstruction :: [Char] -> ([Char], [Char])
splitInstruction = splitAt 1

executeLineA :: Integer -> [Char] -> (Integer, Integer)
executeLineA currentVal line = (newVal, newVal) where
    newVal = mod (currentVal + (instructionToNumber . splitInstruction) line) 100
    
executeFileA :: Integer -> [[Char]] -> Integer
executeFileA initVal lines = toInteger $ length $ filter (== 0) (snd $ mapAccumL executeLineA initVal lines)

executeLineB :: Integer -> [Char] -> (Integer, Integer)
executeLineB currentVal line = (newVal, numClicks)
    where newVal = fst $ executeLineA currentVal line
          diff = (instructionToNumber . splitInstruction) line
          divisor
            | diff < 0 = -100
            | otherwise = 100
          delta = currentVal + (diff `mod` divisor)
          numClicks
            | diff < 0 = (diff `div` divisor) + (toInteger . fromEnum) (delta < 0 && currentVal /= 0)
            | otherwise = (diff `div` divisor) + (toInteger . fromEnum) (delta > 100)

executeFileB :: Integer -> [[Char]] -> Integer
executeFileB initVal lines = sum (snd $ mapAccumL executeLineB initVal lines)

main :: IO()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let answerPartOne = executeFileA 50 $ lines content
    let answerPartTwo = executeFileB 50 $ lines content
    putStrLn $ show answerPartOne ++ " " ++ show answerPartTwo
    print $ answerPartOne + answerPartTwo
