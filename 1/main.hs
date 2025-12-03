import System.Environment
import Data.List

instructionToNumber :: ([Char], [Char]) -> Integer
instructionToNumber ("L", x) = - (read x :: Integer)
instructionToNumber ("R", x) = read x :: Integer

splitInstruction :: [Char] -> ([Char], [Char])
splitInstruction x = (take 1 x, drop 1 x)

executeLineA :: Integer -> [Char] -> (Integer, Integer)
executeLineA currentVal line = (newVal, newVal) where
    newVal = mod (currentVal + ((instructionToNumber . splitInstruction) line)) 100
    
executeFileA :: Integer -> [[Char]] -> Integer
executeFileA initVal lines = toInteger $ length $ filter (\x -> x == 0) (snd $ mapAccumL executeLineA initVal lines)

executeLineB :: Integer -> [Char] -> (Integer, Integer)
executeLineB currentVal line = (newVal, div absValue 100)
    where newVal = fst $ executeLineA currentVal line
          diff = (instructionToNumber . splitInstruction) line
          absValue = abs $ currentVal + diff

executeFileB :: Integer -> [[Char]] -> Integer
executeFileB initVal lines = sum (snd $ mapAccumL executeLineB initVal lines)

main :: IO()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    -- let answerPartOne = executeFileA 50 $ lines content
    -- putStrLn $ show answerPartOne
    -- putStrLn $ show (executeLineA 10 "R990")
    -- putStrLn $ show (executeLineB 10 "R990")
    -- putStrLn $ show (executeLineB 82 "L30")
    -- putStrLn $ show (executeLineB 52 "R48")
    -- putStrLn $ show (executeLineB 0 "L5")
    -- putStrLn $ show (executeLineB 95 "R60")
    -- putStrLn $ show (executeLineB 55 "L55")
    -- putStrLn $ show (executeLineB 0 "L1")
    -- putStrLn $ show (executeLineB 99 "L99")
    -- putStrLn $ show (executeLineB 0 "R14")
    -- putStrLn $ show (executeLineB 14 "L82")
    -- putStrLn $ show (executeLineB 50 "L1000")
    let answerPartTwo = executeFileB 50 $ lines content
    putStrLn $ show answerPartTwo
