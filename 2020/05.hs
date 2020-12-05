{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Control.Exception
import Utils

getInput :: IO [Integer]
getInput = do
	dat <- readFile "05.txt"
	return $ map readPass $ lines dat

readPass :: String -> Integer
readPass = fromBaseN 2 . map charToBinary
	where
		charToBinary 'F' = 0
		charToBinary 'B' = 1
		charToBinary 'L' = 0
		charToBinary 'R' = 1

tests :: IO ()
tests = do
	check $ readPass "BFFFBBFRRR" == 567
	check $ readPass "FFFBBBFRRR" == 119
	check $ readPass "BBFFBBFRLL" == 820
	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	passes <- getInput
	let sorted = sort passes
	print $ last sorted
	print [ x + 1 | (x,y) <- zip sorted (tail sorted), x /= y - 1]
