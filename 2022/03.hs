{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Control.Exception
import Utils

getInput :: IO [String]
getInput = do
	dat <- readFile "03.txt"
	return $ lines dat

rateLetter :: Char -> Integer
rateLetter c
 | 'a' <= c && c <= 'z' = toInteger $ fromEnum c - fromEnum 'a' + 1
 | 'A' <= c && c <= 'Z' = toInteger $ fromEnum c - fromEnum 'A' + 27

partA :: [String] -> Integer
partA = sum . map rate
	where
		rate s = rateLetter c
			where
				n = genericLength s `div` 2
				l = genericTake n s
				r = genericDrop n s
				[c] = nub $ l `intersect` r

partB :: [String] -> Integer
partB = sum . map rate . chunk 3
	where
		rate [x,y,z] = rateLetter c
			where [c] = nub $ x `intersect` y `intersect` z

tests :: IO ()
tests = do
	check $ partA testData == 157
	check $ partB testData == 70
	where
		testData = ["vJrwpWtwJgWrhcsFMMfFFhFp","jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL","PmmdzqPrVvPwwTWBwg","wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn","ttgJtRGJQctTZtZT","CrZsJsPPZsGzwwsLwLmpwMDw"]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	print $ partA dat
	print $ partB dat
