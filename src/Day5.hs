module Day5 where
import Text.Parsec
import Data.List.NonEmpty (append)
import qualified Control.Monad.Identity as Data.Functor.Identity
import Text.Parsec.Token (GenTokenParser(symbol))

parseCrate :: (Monad m) => ParsecT String u m Char
parseCrate = (char '[' *> letter) <* char ']'

parseMoveLine :: Parsec String String (Int, Int, Int)
parseMoveLine = (,,) <$> (parseMove <* char ' ') <*> (parseFrom <* char ' ') <*> parseTo

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
  ([(Char, SourcePos)], [(Int, Int, Int)])
parserAll = do  crates <- concat <$>  parserAllCrates `sepBy` newline
                parseNumbers
                moveLines <- parseMoveLine `sepEndBy` newline
                return (crates, moveLines)
