import Data.Matrix (fromLists, prettyMatrix, submatrix, getElem, Matrix, nrows, ncols, fromList, elementwiseUnsafe)
import System.Environment
import Debug.Trace


replace :: String -> [Int]
replace [] = []
replace (x:xs)
    | x == '@' = 1 : replace xs
    | x == '.' = 0 : replace xs


outerProduct :: [a] -> [b] -> [(a, b)]
outerProduct xs ys =
   do
       x <- xs          -- for each x drawn from xs:
       y <- ys          --   for each y drawn from ys:
       return (x,y)     --      produce the (x,y) pair

checkPatchAt :: Matrix Int -> (Int, Int) -> Int
checkPatchAt mat (r, c)
    -- Quicker cases. If we're out of bounds return 0.
    | r > nr || c > nc = 0
    -- If the center is 0, return 0. No paper to remove.
    | getElem r c mat == 0 = 0
    -- Kinda cheated by using a library to do this, but it would not be hard to do the math to grab things from
    -- A list of lists or a flattened list, even.
    | otherwise = if sum (submatrix patchR0 patchR1 patchC0 patchC1 mat) <= 4 then 1 else 0
    where nr = nrows mat
          nc = ncols mat
          -- Get the coordinates for the top left of the patch and the bottom right of the patch.
          -- We can't go below 1 (since matrices are 1-indexed) or above the number of rows/cols.

          -- (error handling for invalid values is for suckers, not Santa's GOATs)
          patchR0 = if r == 1 then 1 else r - 1
          patchR1 = if r == nr then r else r + 1
          patchC0 = if c == 1 then 1 else c -1
          patchC1 = if c == nc then c else c + 1

resultToMatrixLike :: Matrix Int -> [Int] -> Matrix Int
resultToMatrixLike mat = fromList (nrows mat) (ncols mat)

removeRolls :: Matrix Int -> Matrix Int -> Matrix Int
removeRolls = elementwiseUnsafe f
    where f a b = if a == 1 && b == 1 then 0 else a

recursiveCheck :: Matrix Int -> Int
recursiveCheck mat
    | sum result == 0 = 0
    | otherwise = sum result + recursiveCheck (removeRolls mat result)
    where result = resultToMatrixLike mat $ map (checkPatchAt mat) (outerProduct [1 .. nrows mat] [1 .. ncols mat])

main :: IO()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let linesOfFile = fromLists $ map replace (lines content)
    let result1 = sum $ map (checkPatchAt linesOfFile) (outerProduct [1 .. nrows linesOfFile] [1 .. ncols linesOfFile])
    let result2 = recursiveCheck linesOfFile
    putStrLn $ "Result (Part 1): " ++ show result1
    putStrLn $ "Result (Part 2): " ++ show result2
