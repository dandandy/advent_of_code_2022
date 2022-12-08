module Day7 where
import Text.Parsec
    ( char,
      digit,
      letter,
      newline,
      space,
      string,
      many1,
      sepEndBy,
      (<|>),
      try,
      Parsec )
import Control.Monad.State (State, get, put, runState)
import Data.Map (Map, mapWithKey, empty, insert, toList, map)
import Data.Maybe (mapMaybe)
import qualified Control.Applicative
import Data.List (subsequences, maximumBy, map)

data Stack a = Stack [a] deriving (Show)

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

runState' :: [Cmd] -> ([()], (Stack String, Map String [(Int, String)]))
runState' c = runState (mapM toInfo c) (Stack [], empty)
    where
        toInfo :: Cmd -> State (Stack String, Map String [(Int, String)]) ()
        toInfo cmd = do (stack, files) <- get
                        put (applyCmdToStack cmd stack, applyCmdToMap cmd stack files)
                        return ()


run :: [Cmd] -> Int
run cmds = sum . Data.List.map (snd) $ largestDirsWithAtMost 100000 (sum' map)
    where
        (_,(_, map)) = runState' cmds

        sum' :: Data.Map.Map String [(Int, String)] -> Data.Map.Map String Int
        sum' = Data.Map.map (sum . Data.List.map fst)

largestDirsWithAtMost :: Int -> Map String Int -> [(String, Int)]
largestDirsWithAtMost n = maximumBy compare' . filterLessEqN . filter (not . null) . subsequences  . toList
    where
        filterLessEqN :: [[(String, Int)]] -> [[(String, Int)]]
        filterLessEqN = filter ((<=n) . sum')

        compare' :: [(String, Int)] -> [(String, Int)] -> Ordering
        compare' as bs = compare (sum' as) (sum' bs)

        sum' :: [(String, Int)] -> Int
        sum' = sum . Data.List.map snd

applyCmdToStack :: Cmd -> Stack String -> Stack String
applyCmdToStack (Cd "..") (Stack s) = Stack $ tail s
applyCmdToStack (Cd a) (Stack s) = Stack $ a:s
applyCmdToStack _ s = s

applyCmdToMap :: Cmd -> Stack String -> Map String [(Int, String)] -> Map String [(Int, String)]
applyCmdToMap (Cd "..") _ m = m 
applyCmdToMap (Cd a) _ m = insert a [] m
applyCmdToMap (Ls output) (Stack stack) m = mapWithKey (\k v -> if k `elem` stack then v ++ lsOutput else v) m
    where 
        lsOutput = mapMaybe toFile output

toFile :: LsOutput -> Maybe (Int, String)
toFile (File a b) = Just (a, b)
toFile (Dir _) = Nothing

toDir :: LsOutput -> Maybe String
toDir (File _ _) = Nothing
toDir (Dir a) = Just a
