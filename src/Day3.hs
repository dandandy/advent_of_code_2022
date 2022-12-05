module Day3 where
import Text.Parsec
import Data.Char (ord, isLower, isUpper)
import Data.List

parser :: Parsec String String [String]
parser = many1 letter `sepEndBy` newline

priority :: Char -> Int
priority c | isUpper c = ord c - 65 + 27
priority c | isLower c = ord c - 97 + 1
priority c = error $ "unknown priority " ++ show c

set :: (Ord a) => [a] -> [a]
set a= map head (group $ sort a)

duplicate :: Ord a => [a] -> [a]
duplicate a= map head $ filter ((>1) . length) (group $ sort a)

duplicate3 :: Ord a => [a] -> [a]
duplicate3 a= map head $ filter ((==3) . length) (group $ sort a)

split :: [a] -> ([a], [a])
split a = splitAt (length a `div` 2) a

results :: [String] -> Int
results a = sum $ map result a

result :: String -> Int
result a =sum $ map priority (duplicate (set x ++ set y))
    where
        (x,y) = split a

results2 :: [(String, String, String)] -> Int
results2 = sum . map result2 

result2 :: (String, String, String) -> Int
result2 = sum . map priority  . commonAmong

parse2 :: Parsec String String [(String, String, String)]
parse2 = ((,,) <$> parseLine <*> parseLine <*> many1 letter) `sepEndBy` newline

parseLine :: Parsec String String String
parseLine = many1 letter <* newline

commonAmong :: (String, String, String) -> [Char]
commonAmong (a,b,c) = duplicate3 $ set a ++ set b ++ set c
