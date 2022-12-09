module Day8 where

import Data.Matrix
import Text.Parsec
import Data.List (sort, length, intercalate)
import Data.Vector (splitAt, Vector, toList, length, take, drop)
import qualified Data.Vector.Primitive as Data
import qualified Debug.Trace as Debug

parse' :: Parsec String String (Matrix Int)
parse' = fromLists <$> (many1 (read . (:[]) <$> digit) `sepEndBy` newline)

count' :: Matrix Bool -> Int
count' = Data.List.length . filter (==True) . Data.Matrix.toList

highLight :: Matrix Int -> Matrix Bool
highLight m = Data.Matrix.mapPos (\pos _ -> isVisable pos m) m

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
        row = Debug.trace ("row " ++ intercalate " " [show a])  getRow a' m
        col = Debug.trace ("col " ++ intercalate " " [show b])  getCol b' m
        
start a v = Debug.trace  ("start: " ++ unwords [show a, show v]) (Data.Vector.take a v)
end a v = Debug.trace ("end: " ++ unwords [show a, show v]) ((Data.Vector.drop . (+1)) a v)

isBigger :: Int -> Data.Vector.Vector Int -> Bool
isBigger x xs   | Data.Vector.length xs == 0  = True 
                | otherwise             = Debug.trace ("isBigger: " ++ show x ++ ", " ++ show xs) x > maximum xs
