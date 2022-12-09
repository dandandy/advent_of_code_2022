module Day8 where

import Data.Matrix
import Text.Parsec
import Data.List (sort, length, intercalate)
import Data.Vector (splitAt, Vector, toList, length, take, drop, takeWhile, reverse)
import qualified Data.Vector.Primitive as Data
import qualified Debug.Trace as Debug

parse' :: Parsec String String (Matrix Int)
parse' = fromLists <$> (many1 (read . (:[]) <$> digit) `sepEndBy` newline)

count' :: Matrix Bool -> Int
count' = Data.List.length . filter (==True) . Data.Matrix.toList

highLight :: Matrix Int -> Matrix [[Int]]
highLight m = Data.Matrix.mapPos (\pos _ -> visableFrom pos m) m

isVisable :: (Int, Int) -> Matrix Int -> Bool
-- isVisable (1, b) m = True
-- isVisable (a, 1) m = True
-- isVisable (a, b) m | a == nrows m || b == ncols m = True
isVisable (a', b') m = isBigger' (start b row) || isBigger' (end b row) || isBigger' (start a col) || isBigger' (end a col)
    where
        a = a' - 1
        b=b' - 1
        isBigger' = isBigger el
        el = getElem a' b' m
        row = getRow a' m
        col = getCol b' m

visableFrom :: (Int, Int) -> Matrix Int -> [[Int]]
visableFrom (a', b') m = [canSee' (start b row),canSee' (end b row), canSee' (start a col), canSee' (end a col)]
    where
        a = a' - 1
        b = b' - 1
        canSee' = canSee el
        el = getElem a' b' m
        row = getRow a' m
        col = getCol b' m
        
start a v = Debug.trace  ("start: " ++ unwords [show a, show v]) ((Data.Vector.reverse .  Data.Vector.take a) v)
end a v = (Data.Vector.drop (a+1))  v

isBigger :: Int -> Data.Vector.Vector Int -> Bool
isBigger x xs   | Data.Vector.length xs == 0  = True 
                | otherwise             = Debug.trace ("isBigger: " ++ show x ++ ", " ++ show xs) x > maximum xs

canSee :: Int -> Data.Vector.Vector Int -> [Int]
canSee x = takeTill (<x) (>=x) . Data.Vector.toList 

countTreesCanSee :: [[Int]] -> Int
countTreesCanSee = product . map Data.List.length

takeTill :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeTill p specialStop [] = []
takeTill p specialStop (x:xs)   | specialStop x = [x]
                                | p x = x:takeTill p specialStop xs
                                | otherwise = []

runPart2 :: Matrix Int -> Int
runPart2 = Prelude.maximum . fmap countTreesCanSee <$> highLight
