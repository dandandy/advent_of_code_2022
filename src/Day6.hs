module Day6 where
 
import Data.List
import Text.Parsec

areAllUnique :: String -> Bool
areAllUnique s = length s == length (nub s)

window :: (String -> Bool) -> Int -> String  -> (String,Int)
window p size input = head $ filter (p . fst) $ map (\a -> (slice a (a+size - 1) input, a + size)) [0..(length input - size - 1)]

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

parser :: Parsec String u String
parser = many1 letter
