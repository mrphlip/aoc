{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Control.Exception

getInput :: IO [Integer]
getInput = do
	dat <- readFile "01.txt"
	return $ map read $ lines dat

numIncreases :: [Integer] -> Integer
numIncreases xs = genericLength $ filter (uncurry (<)) $ zip xs (tail xs)

numLongIncreases :: [Integer] -> Integer
numLongIncreases xs = genericLength $ filter (uncurry (<)) $ zip xs (drop 3 xs)

tests :: IO ()
tests = do
	check $ numIncreases values == 7
	check $ numLongIncreases values == 5
	where
		values = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	dat <- getInput
	print $ numIncreases dat
	print $ numLongIncreases dat
