{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Control.Exception
import qualified Text.ParserCombinators.ReadP as P
import Utils

type Range = (Integer, Integer)
type Row = (Range, Range)

getInput :: IO [Row]
getInput = do
	dat <- readFile "04.txt"
	return $ map parseLine $ lines dat

parseLine :: String -> Row
parseLine = runReadP readLine
	where
		readInt = P.readS_to_P reads :: P.ReadP Integer
		readRange = do
			a <- readInt
			P.char '-'
			b <- readInt
			return (a, b)
		readLine = do
			a <- readRange
			P.char ','
			b <- readRange
			return (a, b)

hasSubset :: Row -> Bool
hasSubset ((a,b),(x,y)) = (a <= x && y <= b) || (x <= a && b <= y)
hasIntersect :: Row -> Bool
hasIntersect ((a,b),(x,y)) = a <= y && b >= x

partA :: [Row] -> Integer
partA = genericLength . filter hasSubset
partB :: [Row] -> Integer
partB = genericLength . filter hasIntersect

tests :: IO ()
tests = do
	check $ partA testData == 2
	check $ partB testData == 4
	where
		testData = map parseLine ["2-4,6-8","2-3,4-5","5-7,7-9","2-8,3-7","6-6,4-6","2-6,4-8"]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	print $ partA dat
	print $ partB dat
