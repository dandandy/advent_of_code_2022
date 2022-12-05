module Helper where

import Text.Parsec 
import Data.Functor ((<&>))

runParserStrFilename :: FilePath -> Parsec String String a -> String -> Either ParseError a
runParserStrFilename filename p = runParser p filename filename

runParserStr :: Parsec String String a -> String -> Either ParseError a
runParserStr = runParserStrFilename "unknown" 

runParserFile :: FilePath -> Parsec String String a -> IO (Either ParseError a)
runParserFile filename p = readFile filename <&> runParserStrFilename filename p
