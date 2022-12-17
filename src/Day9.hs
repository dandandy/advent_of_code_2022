module Day9 where
import Text.Parsec (Parsec, letter, space)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char
import Text.Parsec
import Data.List

data Direction = U Int | D Int | L Int | R Int | Noth deriving (Show)

type Pos = (Int, Int)

parse :: Parsec String String [Direction]
parse = parseDirs `sepEndBy` newline

parseDirs :: Parsec String String Direction
parseDirs = toDir <$> (letter <* space) <*> (read <$> many1 digit)

toDir :: Char -> Int -> Direction
toDir 'R' = R
toDir 'U' = U
toDir 'D' = D
toDir 'L' = L

move :: Direction -> Pos ->  [(Direction, Pos)]
move (R d) (x, y) = [(R d, (x + i, y)) | i <- [1..d]]
move (U d) (x, y) = [(U d, (x, y + i)) | i <- [1..d]]
move (L d) (x, y) = [(L d, (x - i, y)) | i <- [1..d]]
move (D d) (x, y) = [(D d, (x, y - i)) | i <- [1..d]]

diff :: Pos -> Pos -> Int
diff (x1, y1) (x2, y2) = max (abs (x2 - x1)) (abs (y2 - y1))

tailPos :: Direction -> Pos -> Pos -> (Direction, Pos)
tailPos _ t h | diff t h <= 1 = (Noth, t)
tailPos Noth t h = (Noth, t)
tailPos (D _) _ (hx, hy) = (D 1, (hx, hy + 1))
tailPos (U _) _ (hx, hy) =(U 1, (hx, hy - 1))
tailPos (L _) _ (hx, hy) = (L 1, (hx + 1, hy))
tailPos (R _) _ (hx, hy) = (R 1, (hx - 1, hy))

calculateTailPos :: Pos -> [(Direction, Pos)] -> [(Direction, Pos)]
calculateTailPos tail [] = []
calculateTailPos tail (d:ds) = newTailPos : calculateTailPos (snd newTailPos) ds
    where
        newTailPos = calculateTailPos' tail d


        calculateTailPos' tail (d, head) = tailPos d tail head

allHeadPos :: [Direction] -> [Pos]
allHeadPos d = (0,0) : map snd (allHeadPos' (0,0) d)


allHeadPos' :: Pos -> [Direction] -> [(Direction, Pos)]
allHeadPos' pos [] = []
allHeadPos' pos [d] = move d pos 
allHeadPos' pos (d:ds) = move d pos ++ allHeadPos' lastPosition ds
    where
        newPositions = move d pos
        lastPosition = snd $ last newPositions

-- run :: [Direction] -> [(Direction, Pos)]
run :: [Direction] -> [Pos]
run d = nub $ map snd $ calculateTailPos (0,0) (allHeadPos' (0,0) d)

knots :: [Direction] -> [[Pos]] -> [Pos]
knots d [] = []
