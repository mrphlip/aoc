{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Control.Exception
import Utils

type Stacks = [[Char]]
type Move = (Integer, Integer, Integer)

getInput :: IO String
getInput = readFile "06.txt"

findSingleton :: Integer -> String -> Integer
findSingleton n s = (+n) $ genericLength $ takeWhile ((/=n).genericLength.nub) $ window n s

tests :: IO ()
tests = do
	check $ map (findSingleton 4) testData == [7, 5, 6, 10, 11]
	check $ map (findSingleton 14) testData == [19, 23, 23, 29, 26]
	where
		testData = [
			"mjqjpqmgbljsphdztnvjfqwrcgsmlb",
			"bvwbjplbgvbhsrlpgdmjqwftvncz",
			"nppdvjthqldpwncqszvftbrmjlhg",
			"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
			"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	print $ findSingleton 4 dat
	print $ findSingleton 14 dat
