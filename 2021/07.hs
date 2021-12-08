{-# OPTIONS_GHC -Wno-tabs #-}
import Control.Exception
import Utils

getInput :: IO [Integer]
getInput = do
	dat <- readFile "07.txt"
	return $ map read $ split ',' dat

metricA :: Integer -> Integer -> Integer
metricA a b = abs (a - b)
metricB :: Integer -> Integer -> Integer
metricB a b = d * (d+1) `div` 2
	where d = abs (a - b)

optimise metric vals = minimum $ map calcCost $ [minimum vals .. maximum vals]
	where calcCost target = sum $ map (metric target) vals

tests :: IO ()
tests = do
	check $ optimise metricA vals == 37
	check $ optimise metricB vals == 168
	where
		vals = [16,1,2,0,4,2,7,1,2,14]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	vals <- getInput
	print $ optimise metricA vals
	print $ optimise metricB vals
