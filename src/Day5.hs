module Day5 where
import Text.Parsec
import Data.List.NonEmpty (append, insert)
import qualified Control.Monad.Identity as Data.Functor.Identity
import Text.Parsec.Token (GenTokenParser(symbol), makeTokenParser)
import qualified Data.Map as Data
import Text.Parsec.Pos

data Move = Move Int Int Int deriving (Show)

parseCrate :: (Monad m) => ParsecT String u m Char
parseCrate = (char '[' *> letter) <* char ']'

parseMoveLine :: Parsec String String Move
parseMoveLine = Move <$> (parseMove <* char ' ') <*> (parseFrom <* char ' ') <*> parseTo

parseMove :: Parsec String String Int
parseMove = string "move " *> (read <$> many1 digit) 

parseFrom :: Parsec String String Int
parseFrom  = string "from " *> (read <$> many1 digit)

parseTo :: Parsec String String Int
parseTo  = string "to " *> (read <$> many1 digit)

parserCratePos :: Monad m => ParsecT String u m (Char, SourcePos)
parserCratePos = do value <- parseCrate
                    pos <- getPosition
                    return (value, pos)


spaces' :: Monad m => ParsecT String u m ()
spaces' = skipMany (char ' ')

parserAllCrates :: Monad m => ParsecT String u m [(Char, SourcePos)]
parserAllCrates = spaces' *> (parserCratePos `sepEndBy` spaces')

parseNumbers :: Monad m => ParsecT String u m [Int]
parseNumbers = (read <$> many1 digit) `sepEndBy1` spaces

parserAll :: ParsecT
  String
  String
  Data.Functor.Identity.Identity
  ([(Char, SourcePos)], [Move])
parserAll = do  crates <- concat <$>  parserAllCrates `sepBy` newline
                parseNumbers
                moveLines <- parseMoveLine `sepEndBy` newline
                return (crates, moveLines)

buildMap :: [(Char, SourcePos)] -> Data.Map Int [Char]
buildMap ls = snd $ inserting (ls, Data.empty)

inserting :: ([(Char, SourcePos)], Data.Map Int [Char]) -> ([(Char, SourcePos)], Data.Map Int [Char])
inserting ([], m) = ([], m)
inserting ((c, s):ls, m) = inserting (ls, Data.insertWith (++) (sourceColumn s `div` 4) [c] m)

result :: ([(Char, SourcePos)], [Move]) -> Data.Map Int [Char]
result (cs, ms) = moving ms $ buildMap cs

tops :: Data.Map Int [Char] -> [Char]
tops m = map (last . snd) $ Data.toList m

moving :: [Move] -> Data.Map Int [Char] -> Data.Map Int [Char]
moving ls m = foldl (flip move) m ( ls)
-- moving (l:l2:l3:l4:ls) m = move (head ls) $  move l4 $  move l3 $ move l2 $ move l m

move :: Move -> Data.Map Int [Char] -> Data.Map Int [Char]
move (Move amount from to) m | amount == 0 = m
                             | otherwise = move (Move (amount - 1) from to) (push to (pop from m))

pop :: Int -> Data.Map Int [Char] -> (Data.Map Int [Char], Char)
pop i m = (Data.insert i (init list) m, last list)
  where 
    list = m Data.! i 

push :: Int -> (Data.Map Int [Char], Char) -> Data.Map Int [Char]
push i (m, c) = Data.insertWith (flip (++)) i [c] m
