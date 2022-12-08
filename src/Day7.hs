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
import Control.Monad.State (State, get, put, execState, evalState)
import Control.Monad (when)
import Data.Map (Map, mapWithKey, empty, insert, toList, map)
import Data.Maybe (mapMaybe, catMaybes)
import qualified Control.Applicative
import Data.List (subsequences, maximumBy, map, sortBy)
import Debug.Trace ( trace )

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

runState' :: [Cmd] -> (Stack String, Map String [(Int, String)])
runState' c =execState (mapM toInfo c) (Stack [], empty)
    where
        toInfo :: Cmd -> State (Stack String, Map String [(Int, String)]) ()
        toInfo cmd = do (stack, files) <- get
                        put ((applyCmdToStack cmd stack), applyCmdToMap cmd stack files)
                        return ()

delete :: [Int] -> Int
delete dirs = (minimum . filter (>=requiredSizeToFree)) dirs
    where
        max = maximum  dirs
        unusedSpace = 70000000 - max
        requiredSizeToFree = 30000000 - unusedSpace

run :: [Cmd] -> Int
run cmds = delete $ Data.List.map snd $ toList $ sum' map
    where
        (_,map) = runState' cmds

sum' :: Data.Map.Map String [(Int, String)] -> Data.Map.Map String Int
sum' = Data.Map.map (sum . Data.List.map fst)

takeWhile' :: Int -> [(String,Int)] -> [(String, Int)]
takeWhile' n strs = catMaybes $  evalState (mapM (takeWhileState n) strs) 0

takeWhileState :: Int -> (String, Int) -> State Int (Maybe (String, Int))
takeWhileState n (str, int) =  do   x <- get
                                    put $ x + int
                                    if x + int <= n then 
                                        return $ Just (str, int) 
                                        else return Nothing


applyCmdToStack :: Cmd -> Stack String -> Stack String
applyCmdToStack (Cd "..") (Stack s) = Stack $ tail s
applyCmdToStack (Cd a) (Stack s) = Stack $ a:s
applyCmdToStack _ s = s

applyCmdToMap :: Cmd -> Stack String -> Map String [(Int, String)] -> Map String [(Int, String)]
applyCmdToMap (Cd "..") _ m = m 
applyCmdToMap (Cd a) _ m = insert a [] m
applyCmdToMap (Ls output) (Stack stack) m = mapWithKey (\k v -> if k `elem` stack then lsOutput++v else v) m
    where 
        lsOutput = mapMaybe toFile output

toFile :: LsOutput -> Maybe (Int, String)
toFile (File a b) = Just (a, b)
toFile (Dir _) = Nothing

toDir :: LsOutput -> Maybe String
toDir (File _ _) = Nothing
toDir (Dir a) = Just a
