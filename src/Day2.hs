module Day2 where
import Text.Parsec

data RPS = Rock | Paper | Scissors deriving (Show)

data Outcome = Win | Loss | Draw deriving (Show)


matchOutcome :: RPS -> RPS -> Outcome
matchOutcome Rock Paper = Loss
matchOutcome Rock Rock = Draw
matchOutcome Rock Scissors = Win
matchOutcome Paper Paper = Draw
matchOutcome Paper Rock = Win
matchOutcome Paper Scissors = Loss
matchOutcome Scissors Scissors = Draw
matchOutcome Scissors Rock = Loss
matchOutcome Scissors Paper = Win

outcomePoints :: Outcome -> Int
outcomePoints Win = 6
outcomePoints Loss = 0
outcomePoints Draw = 3

shapePoints :: RPS -> Int
shapePoints Rock = 1
shapePoints Paper = 2
shapePoints Scissors = 3

code :: Char -> RPS
code 'A' = Rock
code 'B' = Paper
code 'C' = Scissors

strategyGuide :: Char -> RPS
strategyGuide 'X' = Rock
strategyGuide 'Y' = Paper
strategyGuide 'Z' = Scissors

parsePart1 :: Parsec String String [(Char, Char)]
parsePart1 = ((,) <$> (letter <* space) <*> letter) `sepEndBy` newline 

newStrategyGuide :: Char -> RPS -> RPS
newStrategyGuide 'X' Rock = Scissors
newStrategyGuide 'X' Paper = Rock
newStrategyGuide 'X' Scissors = Paper
newStrategyGuide 'Y' Rock = Rock
newStrategyGuide 'Y' Paper = Paper
newStrategyGuide 'Y' Scissors = Scissors
newStrategyGuide 'Z' Rock = Paper
newStrategyGuide 'Z' Paper = Scissors
newStrategyGuide 'Z' Scissors = Rock

runPart1 :: [(Char, Char)] -> Int
runPart1 =sum . map (calculateScore .  toRPS)

runPart2 :: [(Char, Char)] -> Int
runPart2 = sum . map (calculateScore . toRPS2)

toRPS :: (Char, Char) -> (RPS, RPS)
toRPS (a,b) = (code a, strategyGuide b)

calculateScore :: ( RPS, RPS) -> Int
calculateScore (a, b) = shapePoints b +  outcomePoints (matchOutcome b a)

toRPS2 :: (Char, Char) -> (RPS, RPS)
toRPS2 (a,b) = (code a, newStrategyGuide b (code a))
