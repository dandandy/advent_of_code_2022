module Day7 where
import Text.Parsec

data FileTree = FileTree String [(Int, String)] [FileTree] deriving (Show)

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

buildTree :: [Cmd] -> FileTree
buildTree ((Cd "/"):cmd) = buildTree' (FileTree "/" [] []) cmd
buildTree _ = error "needs to start from /"

buildTree' :: FileTree -> [Cmd] -> FileTree
buildTree' f [] = f
buildTree' f ((Cd ".."):cmd) = f   
buildTree' f ((Cd "/"):cmd) = error "cannot return to root"   
buildTree' (FileTree name files dirs) ((Cd dir):cmd) = FileTree name files ((buildTree' (head $ filter ((==dir) . (\(FileTree name _ _) -> name)) dirs ) cmd):(filter ((/=dir) . (\(FileTree name _ _) -> name)) dirs))
buildTree' (FileTree name files dirs) ((Ls lsOutput):cmd) = buildTree' (FileTree name (files ++ concatMap toFile lsOutput) (dirs ++ concatMap toDir lsOutput)) cmd

toFile :: LsOutput -> [(Int, String)]
toFile (File a b) = [(a, b)]
toFile (Dir _) = []

toDir :: LsOutput -> [FileTree]
toDir (File _ _) = []
toDir (Dir a) = [FileTree a [] []]
