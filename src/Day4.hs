module Day4 where
import Text.Parsec
import Data.List.NonEmpty (cons)

parse :: Parsec String u [((Int, Int), (Int, Int))]
parse = ((,) <$> (parsePoint <* char ',') <*> parsePoint) `sepEndBy` newline

parsePoint :: Parsec String u (Int, Int)
parsePoint = (,) <$> (parseDigit <* char '-') <*> parseDigit

parseDigit :: Parsec String u Int
parseDigit = read <$> many1 digit

isOverLapping :: ((Int, Int), (Int, Int)) -> Bool
isOverLapping ((a,b),(x,y)) | b >= x && a <= y = True
                            | a <= y && b >= y  = True
                            | otherwise = False

result :: [((Int, Int), (Int, Int))] -> Int
result = length . filter isOverLapping
