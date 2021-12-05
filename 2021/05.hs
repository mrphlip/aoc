{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import qualified Data.Map.Strict as M
import qualified Text.ParserCombinators.ReadP as P
import Control.Exception
import Control.Monad
import Utils

type Point = (Integer, Integer)
type Line = (Point, Point)

getInput :: IO [Line]
getInput = do
	dat <- readFile "05.txt"
	return $ map parseInputLine $ lines dat
parseInputLine :: String -> Line
parseInputLine line = runReadP readLine line
	where
		readInt = P.readS_to_P reads :: P.ReadP Integer
		readLine :: P.ReadP Line
		readLine = do
			P.skipSpaces
			x1 <- readInt
			P.skipSpaces
			P.char ','
			P.skipSpaces
			y1 <- readInt
			P.skipSpaces
			P.string "->"
			P.skipSpaces
			x2 <- readInt
			P.skipSpaces
			P.char ','
			P.skipSpaces
			y2 <- readInt
			P.skipSpaces
			return ((x1, y1), (x2, y2))

isCardinal :: Line -> Bool
isCardinal ((x1, y1), (x2, y2))
	| x1 == x2 = True
	| y1 == y2 = True
	| otherwise = False

makeRange :: Integer -> Integer -> [Integer]
makeRange x y
	| x <= y = [x..y]
	| x > y = [y..x]

traceLine :: Line -> [Point]
traceLine ((x1, y1), (x2, y2))
	| x1 == x2 = [(x1, y) | y <- makeRange y1 y2] 
	| y1 == y2 = [(x, y1) | x <- makeRange x1 x2]
	| (x2 - x1) * slope == (y2 - y1) = [(x, (x - x1) * slope + y1) | x <- makeRange x1 x2]
	| otherwise = error "Line is not cardinal or diagonal"
	where slope = if (x1 < x2) == (y1 < y2) then 1 else -1

countPoints :: [Line] -> M.Map Point Integer
countPoints lines = counter $ join $ map traceLine lines

countIntersections :: [Line] -> Integer
countIntersections lines = genericLength $ filter ((>1).snd) $ M.toList $ countPoints lines

tests :: IO ()
tests = do
	check $ countIntersections (filter isCardinal lines) == 5
	check $ countIntersections lines == 12
	where
		lines = map parseInputLine ["0,9 -> 5,9","8,0 -> 0,8","9,4 -> 3,4","2,2 -> 2,1","7,0 -> 7,4","6,4 -> 2,0","0,9 -> 2,9","3,4 -> 1,4","0,0 -> 8,8","5,5 -> 8,2"]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	lines <- getInput
	print $ countIntersections $ filter isCardinal lines
	print $ countIntersections lines
