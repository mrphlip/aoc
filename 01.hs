{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Control.Exception

calcFuel :: Integer -> Integer
calcFuel x = (x `div` 3) - 2

calcFuelFuel :: Integer -> Integer
calcFuelFuel x = sum $ unfoldr iterfunc x
	where iterfunc x = let f = calcFuel x in if f > 0 then Just (f, f) else Nothing

getInput :: IO [Integer]
getInput = do
	dat <- readFile "1.txt"
	return $ map read $ lines dat

tests :: IO ()
tests = do
	check $ calcFuel 12 == 2
	check $ calcFuel 14 == 2
	check $ calcFuel 1969 == 654
	check $ calcFuel 100756 == 33583
	check $ calcFuelFuel 14 == 2
	check $ calcFuelFuel 1969 == 966
	check $ calcFuelFuel 100756 == 50346
	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	weights <- getInput
	print $ sum $ map calcFuel weights
	print $ sum $ map calcFuelFuel weights
