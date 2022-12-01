{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Data.List.Split (splitOn)
import Control.Exception

getInput :: IO [[Integer]]
getInput = do
	dat <- readFile "01.txt"
	return $ parseInput dat

parseInput :: String -> [[Integer]]
parseInput dat = map (map read . lines) $ splitOn "\n\n" dat

sortElves :: [[Integer]] -> [[Integer]]
sortElves = reverse . sortOn sum

findTop :: Integer -> [[Integer]] -> Integer
findTop n = sum . map sum . genericTake n . sortElves

tests :: IO ()
tests = do
	check $ findTop 1 testData == 24000
	check $ findTop 3 testData == 45000
	where
		testData = parseInput "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	print $ findTop 1 dat
	print $ findTop 3 dat
