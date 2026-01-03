import System.Environment (getArgs)
import Data.List (sortBy, genericLength, minimumBy)
import Data.Ord (comparing)
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import GHC.Generics
import Data.Either
import Debug.Trace
import qualified Data.HashMap.Strict as HMS
import Data.Hashable
import Control.Monad.ST.Strict (runST)



data Point3D = Point3D { x :: Double, y :: Double, z :: Double } deriving (Show, Eq, Generic)

class Metric p where
    distance :: p -> p -> Double

instance Metric Point3D where
    distance  (Point3D x1 y1 z1) (Point3D x2 y2 z2) = 
        sqrt ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)
instance FromRecord Point3D where
    parseRecord v
        | length v == 3 = Point3D <$> v .! 0 <*> v .! 1 <*> v .! 2
        | otherwise = fail "Expected 3 values for point."
instance Hashable Point3D where
    hashWithSalt salt (Point3D x y z) = hashWithSalt salt (x, y, z)


data Tree a = Node a (Tree a) (Tree a) | Empty
data KDTree a b = KDTree (Accessors a b) (Tree a)

type Accessors a b = [a -> b]

access3D :: [Point3D -> Double]
access3D = [d1, d2, d3]
    where d1 (Point3D a _ _) = a
          d2 (Point3D _ b _) = b
          d3 (Point3D _ _ c) = c


sqrDist :: Num b => Accessors a b -> a -> a -> b
sqrDist dims a b = sum $ map square $ zipWith (-) a' b'
  where
    a' = map ($ a) dims
    b' = map ($ b) dims

square :: Num a => a -> a
square = (^ 2)

fromList :: Ord b => Accessors a b -> [a] -> KDTree a b
fromList dims values = KDTree dims $ fromList' (cycle dims) values
    where
        fromList' _ [] = Empty
        fromList' (d:ds) values =
            let sorted = sortBy (comparing d) values
                (lower, higher) = splitAt (genericLength sorted `div` 2) sorted
            in case higher of
                [] -> Empty
                median:rest -> Node median (fromList' ds lower) (fromList' ds rest)

nearest :: (Ord b, Num b, Integral c) => KDTree a b -> a -> (Maybe a, c)
nearest (KDTree dims tree) value = near (cycle dims) tree
    where
        dist = sqrDist dims
        near _ Empty = (Nothing, 1)
        near _ (Node split Empty Empty) = (Just split, 1)
        near (d:ds) (Node split left right) =
            let dimDist x y = square (d x - d y)
                splitDist = dist value split
                hyperPlaneDist = dimDist value split
                bestLeft = near ds left
                bestRight = near ds right

                ((maybeThisBest, thisCount), (maybeOtherBest, otherCount)) =
                    if d value < d split
                    then (bestLeft, bestRight)
                    else (bestRight, bestLeft)
            in case maybeThisBest of
                Nothing ->
                    let count = 1 + thisCount + otherCount
                    in case maybeOtherBest of
                        Nothing -> (Just split, count)
                        Just otherBest ->
                            if dist value otherBest < splitDist
                            then (maybeOtherBest, count)
                            else (Just split, count)
                Just thisBest ->
                    let thisBestDist = dist value thisBest
                        best = if splitDist < thisBestDist
                               then split else thisBest
                        bestDist = dist value best
                    in
                        if bestDist < hyperPlaneDist
                        then (Just best, 1 + thisCount)
                        else
                            let count = 1 + thisCount + otherCount
                            in case maybeOtherBest of
                                Nothing -> (Just best, count)
                                Just otherBest ->
                                    if bestDist < dist value otherBest
                                    then (Just best, count)
                                    else (maybeOtherBest, count)

nearestWith :: (Eq a, Ord b, Num b, Integral c) => KDTree a b -> a -> (a -> Bool) -> (Maybe a, Maybe b, c)
nearestWith (KDTree dims tree) value exclude = near (cycle dims) tree
    where
        dist = sqrDist dims
        near _ Empty = (Nothing, Nothing, 1)
        near _ (Node split Empty Empty) = if exclude split then (Nothing, Nothing, 1) else (Just split, Just (sqrDist dims value split), 1)
        near (d:ds) (Node split left right) =
            let dimDist x y = square (d x - d y)
                splitDist = dist value split
                hyperPlaneDist = dimDist value split
                (maybeThisBest, thisbestDist, thisCount) = near ds left
                (maybeOtherBest, otherbestDist, otherCount) = near ds right

                ((maybeThisBest', thisBestDist', thisCount'), (maybeOtherBest', otherBestDist', otherCount')) =
                    if d value < d split
                    then ((maybeThisBest, thisbestDist, thisCount), (maybeOtherBest, otherbestDist, otherCount))
                    else ((maybeOtherBest, otherbestDist, otherCount), (maybeThisBest, thisbestDist, thisCount))
                
                isSplitValid = not (exclude split)
            in case maybeThisBest' of
                Nothing ->
                    let count = 1 + thisCount' + otherCount'
                    in case maybeOtherBest' of
                        Nothing -> if isSplitValid then (Just split, Just splitDist, count) else (Nothing, Nothing, count)
                        Just otherBest ->
                            if dist value otherBest < splitDist && not (exclude otherBest)
                            then (maybeOtherBest', otherBestDist', count)
                            else if isSplitValid then (Just split, Just splitDist, count) else (maybeOtherBest', otherBestDist', count)
                Just thisBest ->
                    let thisBestDist = dist value thisBest
                        best = if isSplitValid && splitDist < thisBestDist
                               then split else thisBest
                        bestDist = dist value best
                    in
                        if bestDist < hyperPlaneDist
                        then (Just best, Just bestDist, 1 + thisCount')
                        else
                            let count = 1 + thisCount' + otherCount'
                            in case maybeOtherBest' of
                                Nothing -> (Just best, Just bestDist, count)
                                Just otherBest ->
                                    if bestDist < dist value otherBest
                                    then (Just best, Just bestDist, count)
                                    else (maybeOtherBest', otherBestDist', count)

-- Write a second blacklist function that takes the value and the split and returns false if:
    -- The value equals the split, or
    -- The value is directly connected to the split (tracked by a hash map.)


-- solve :: BL.ByteString -> [(Point3D, Point3D)]
-- solve content = go points blMap 0
--     where
--         points = V.toList $ fromRight V.empty (decode NoHeader content :: Either String (V.Vector Point3D))
--         tree = fromList access3D points
--         blMap :: HMS.HashMap Point3D [Point3D] = HMS.empty
--         go (p:ps) blMap i = let
--             (maybeN, _) = nearestWith tree p (createBlacklistMap blMap p)
--             in case maybeN of
--                 Nothing -> error "this shouldn't happen"
--                 Just n ->
--                     if i > 9 then [(p, n)]
--                     else (p, n) : go ps (HMS.insertWith (++) p [n] blMap) (i + 1)
--                     -- HMS.insertWith (++) p [n]

-- solve :: BL.ByteString -> [(Point3D, Point3D)]
-- solve content = go points blMap 0
--     where
--         points = V.toList $ fromRight V.empty (decode NoHeader content :: Either String (V.Vector Point3D))
--         tree = fromList access3D points
--         blMap :: HMS.HashMap Point3D [Point3D] = HMS.empty


createBlacklistMap :: (Eq a, Hashable a) => HMS.HashMap a [a] -> a -> (a -> Bool)
createBlacklistMap bl val = 
    let maybeBls = HMS.lookup val bl
    in case maybeBls of
        Nothing -> (== val)
        Just bls -> (\x -> (x `elem` bls) || (x == val))

main :: IO()
main = do
    args <- getArgs
    content <- BL.readFile (args !! 0)
    let points = decode NoHeader content :: Either String (V.Vector Point3D)
    let tree = fromList access3D (V.toList $ fromRight V.empty points)
    let blMap = createBlacklistMap HMS.empty (Point3D 162 817 812)
    -- print $ nearest tree (Point3D 162 817 812)
    -- print $ solve content
    print $ nearestWith tree (Point3D 162 817 812) blMap
    print $ sqrDist access3D (Point3D 162 817 812) (Point3D 425 690 689)
    -- print content