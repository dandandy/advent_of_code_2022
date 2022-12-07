module Day7 where
import Text.Parsec

data Cmd = Cd String | Ls [LsOutput] deriving (Show)

data LsOutput = Dir String | File Int String deriving (Show)

parseCmd :: Parsec String String [Cmd]
parseCmd = many1 (try parseCd <|> try parseLs)

parseCd :: Parsec String String Cmd
parseCd = Cd <$> (string "$ cd " *> (try (many1 letter) <|> try (string "..") <|> try (string "/")) <* newline)

parseLs :: Parsec String String Cmd
parseLs = Ls <$> (string "$ ls" *> newline *> parseLsOutput)

parseLsOutput :: Parsec String String [LsOutput]
parseLsOutput = ((try parseDir <|> try parseFileLine))  `sepEndBy` newline

parseDir :: Parsec String String LsOutput
parseDir = Dir <$> (string "dir " *> many1 letter)

parseFileLine :: Parsec String String LsOutput
parseFileLine = pure File <*> (parseInt <* space) <*> parseFileName
    where
        parseFileName :: Parsec String String String
        parseFileName = many1 (try letter <|> try (char '.'))

        parseInt :: Parsec String String Int
        parseInt = read <$> many1 digit
