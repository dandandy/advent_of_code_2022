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
      Parsec, many )
import Control.Monad.State (State, get, put, execState, evalState)
import Control.Monad (when)
import Data.Map (Map, mapWithKey, empty, insert, toList, map, lookup)
import Data.Maybe (mapMaybe, catMaybes)
import qualified Control.Applicative
import Data.List (subsequences, maximumBy, map, sortBy, intercalate)
import Debug.Trace ( trace )
import Control.Monad (join)

data Stack a = Stack [a] deriving (Show)

data Cmd = Cd String | Ls [LsOutput] deriving (Show)

data LsOutput = Dir String | File Int String deriving (Show)

type FileSystem = Data.Map.Map Dir ([File], [Dir])

type Dir = String

type File = (Int, String)

parseCmd :: Parsec String String [Cmd]
parseCmd = many1 (try parseCd <|> try parseLs)

parseCd :: Parsec String String Cmd
parseCd = Cd <$> (string "$ cd " *> (try (many1 letter) <|> try (string "..") <|> try (string "/")) <* newline)

parseLs :: Parsec String String Cmd
parseLs = Ls <$> (string "$ ls" *> newline *> parseLsOutput)

parseLsOutput :: Parsec String String [LsOutput]
parseLsOutput = (try parseDir <|> try parseFileLine)  `sepEndBy` newline

parseDir :: Parsec String String LsOutput
parseDir = Dir <$> (string "dir " *> many1 letter)

parseFileLine :: Parsec String String LsOutput
parseFileLine = pure File <*> (parseInt <* space) <*> parseFileName
    where
        parseFileName :: Parsec String String String
        parseFileName = many1 (try letter <|> try (char '.'))

        parseInt :: Parsec String String Int
        parseInt = read <$> many1 digit

runState' :: [Cmd] -> (Stack String, FileSystem)
runState' c = execState (mapM toInfo c) (Stack [], empty)
    where
        toInfo :: Cmd -> State (Stack String, FileSystem) ()
        toInfo cmd = do (stack, files) <- get
                        put (applyCmdToStack cmd stack, applyCmdToMap cmd stack files)
                        return ()

applyCmdToStack :: Cmd -> Stack String -> Stack String
applyCmdToStack (Cd "..") (Stack s) = Stack $ tail s
applyCmdToStack (Cd a) (Stack s) = Stack $ a:s
applyCmdToStack _ s = s

applyCmdToMap :: Cmd -> Stack String -> FileSystem -> FileSystem
applyCmdToMap (Cd "..") _ m = m 
applyCmdToMap (Cd a) (Stack stack) m = m
applyCmdToMap (Ls output) (Stack stack) m = insert (stackPath (Stack stack)) (files, dirPaths) m
    where 
        files = mapMaybe toFile output
        dirNames = mapMaybe toDir output
        dirPaths = Data.List.map (stackPath . Stack . (:stack)) dirNames

allSubStackPaths :: Stack String -> [String]
allSubStackPaths (Stack str) = [stackPath (Stack (drop s str)) | s <- [0.. length str - 1]]

stackPath :: Stack String -> String
stackPath (Stack str) = intercalate "/" str

-- run :: [Cmd] -> Int 
run cmds = sum . Data.List.map snd $ filter ((<=100000) . snd) $ toList $ sum' map
    where
        (_,map) = runState' cmds

sum' :: FileSystem -> Data.Map.Map String Int
sum' m = mapWithKey (\k (_,dirs) -> lookup' (k:dirs) dirSizes) m
    where
        dirSizes = Data.Map.map (sum . Data.List.map fst . fst) m

        lookup' :: [Dir] -> Data.Map.Map Dir Int -> Int
        lookup' dirs m = sum $ mapMaybe (`Data.Map.lookup` m) dirs

toFile :: LsOutput -> Maybe File
toFile (File a b) = Just (a, b)
toFile (Dir _) = Nothing

toDir :: LsOutput -> Maybe String
toDir (File _ _) = Nothing
toDir (Dir a) = Just a
