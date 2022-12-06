module Day6 where
 
import Data.List
import Text.Parsec

part12 :: Int -> String -> Int 
part12 len str  = head $ [i+ len  | i <- [0..length str - len], length ( nub $ slice i (i+len) str) == len]

parser :: Parsec String u String
parser = many1 letter

slice :: Int -> Int -> [a] -> [a]
slice a b ls = map snd $ filter ((\v -> v>=a && v<b) . fst) $ zip [0..] ls 
