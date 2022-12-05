module Day1 where
import Text.Parsec 
import qualified Control.Monad.Identity as Monad
import Text.Parsec.Char (digit)
import Data.List (elemIndex, sort)
import Text.Parsec.String (parseFromFile)


day1part1Main = do   x <- readFile "day1part1.txt" 
                     return $ day1part1 x

day1part1Example :: IO (Either ParseError String)
day1part1Example = do   x <- readFile "example1.txt" 
                        return $ day1part1 x

resultPart2 :: [[Int]] -> Int
resultPart2 arr = sum $ take 3 $ reverse $ sort $ map sum arr

day1part1 :: String -> Either ParseError String
day1part1 a= show .  resultPart2 <$> runParser parserDay1 "" "" a

resultDay1part1 :: [[Int]] -> Int
resultDay1part1 arr = maxValue
    where
        maxList = map sum arr
        maxValue = maximum maxList


parserDay1 :: (Monad m) => ParsecT String u m [[Int]]
parserDay1 =  many1 (parserDay1Int <* newline) `sepBy` newline

parserDay1Int :: (Monad m) => ParsecT String u m Int
parserDay1Int = read <$> many1 digit


example1 = "1000\n\
\2000\n\
\3000\n\
\\n\
\4000\n\
\\n\
\5000\n\
\6000\n\
\\n\
\7000\n\
\8000\n\
\9000\n\
\\n\
\10000\n"
