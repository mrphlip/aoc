{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Control.Exception

getInput :: IO [Integer]
getInput = do
	dat <- readFile "10.txt"
	return $ sort $ map read $ lines dat

addLimits :: [Integer] -> [Integer]
addLimits xs = [0] ++ xs ++ [(last xs) + 3]

getDiffs :: [Integer] -> [Integer]
getDiffs xs = map (uncurry (-)) $ zip (tail xs) xs

getPartA :: [Integer] -> Integer
getPartA diffs = ones * threes
	where
		ones = genericLength $ filter (==1) diffs
		threes = genericLength $ filter (==3) diffs

getPartB :: [Integer] -> Integer
getPartB diffs = head result
	where
		result = map solveFor [0..length diffs - 1]
		maxstep n = subtract 1 $ length $ takeWhile (<=3) $ scanl (+) 0 $ drop n diffs
		solveFor n
			| n == length diffs - 1 = 1
			| otherwise = sum $ take (maxstep n) $ drop (n+1) result

tests :: IO ()
tests = do
	check $ getPartA diffs == 220
	check $ getPartB diffs == 19208
	where
		testdata = sort [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]
		diffs = getDiffs $ addLimits testdata
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	vals <- getInput
	let diffs = getDiffs $ addLimits vals
	print $ getPartA diffs
	print $ getPartB diffs
