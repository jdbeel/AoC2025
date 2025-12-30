{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}


import System.Environment (getArgs)
import Data.List (elemIndex, nub, foldl')
import Data.Maybe
import GHC.Generics
import Debug.Trace
import qualified Data.List.NonEmpty as NonEmpty (fromList, head, toList)
import Control.Monad.ST.Strict (runST)
import Data.Vector.Primitive.Mutable qualified as MVector
import Data.ByteString qualified as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char qualified as Char
import Data.Word (Word8)
import Control.Monad (foldM)




sendBeam :: ([[Bool]], (Int, Int)) -> [(Int, Int)]
sendBeam (grid, (y, x))
    | y == length grid = []
    | x > 1 && b !! x && b !! (x - 2) = (y, x) : sendBeam (grid, (y + 1, x + 1))
    | b !! x = (y, x) : sendBeam (grid, (y + 1, x + 1)) ++ sendBeam (grid, (y + 1, x - 1))
    | otherwise = sendBeam (grid, (y + 1, x))
    where b = grid !! y

solve :: [BS.ByteString] -> (Int, Int)
solve [] = error "no input"
solve (l:ls) = runST $ do
    timelines <- MVector.replicate width 0
    MVector.write timelines start 1
    let go !acc !i = do
          t <- MVector.read timelines i
          if t == 0
            then pure acc
            else do
              MVector.write timelines i 0
              MVector.modify timelines (+ t) (i - 1)
              MVector.modify timelines (+ t) (i + 1)
              pure $ acc + 1
    hits <- foldM go 0 splitters
    ts <- MVector.foldl' (+) 0 timelines
    pure (hits, ts)
    where
        width = BS.length l
        start = fromMaybe (error "no start") $ BS.elemIndex(c2w 'S') l
        splitters = concatMap (BS.elemIndices (c2w '^')) $ everyOther ls
        everyOther = \case
            _ : x : xs -> x : everyOther xs
            _ -> []

c2w :: Char -> Word8
c2w = fromIntegral . Char.ord

main :: IO()
main = do
    args <- getArgs
    content <- readFile (args !! 0)

    let lines' = NonEmpty.fromList $ lines content
    let startLine = NonEmpty.head lines'
    let bitmap = map (map (== '^')) (drop 1 $ NonEmpty.toList lines')
    let startIndex = fromJust $ elemIndex 'S' startLine
    let beams = curry sendBeam bitmap (0, startIndex)
    let resultPartOne = length $ nub beams
    print resultPartOne

    let bsLines = map (TE.encodeUtf8 . T.pack) $ NonEmpty.toList lines' 
    let resultPartTwo = solve bsLines
    print resultPartTwo
