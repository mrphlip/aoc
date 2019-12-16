{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Char
import Data.List
import Utils

getInput :: IO [Integer]
getInput = do
	dat <- readFile "16.txt"
	return $ map (toInteger.digitToInt) $ takeWhile (/='\n') dat

pattern :: Integer -> [Integer]
pattern n = tail $ cycle (rep 0 ++ rep 1 ++ rep 0 ++ rep (-1))
	where rep x = genericTake n $ repeat x

phase :: [Integer] -> [Integer]
phase xs = map getPos [1..genericLength xs]
	where getPos n = (`mod` 10) $ abs $ sum $ zipWith (*) xs $ pattern n

manyPhases :: Integer -> [Integer] -> [Integer]
manyPhases n xs = iterate phase xs `genericIndex` n

tests :: IO ()
tests = do
	test $ phase [1,2,3,4,5,6,7,8] == [4,8,2,2,6,1,5,8]
	test $ phase [4,8,2,2,6,1,5,8] == [3,4,0,4,0,4,3,8]
	test $ phase [3,4,0,4,0,4,3,8] == [0,3,4,1,5,5,1,8]
	test $ phase [0,3,4,1,5,5,1,8] == [0,1,0,2,9,4,9,8]

	test $ (take 8 $ manyPhases 100 [8,0,8,7,1,2,2,4,5,8,5,9,1,4,5,4,6,6,1,9,0,8,3,2,1,8,6,4,5,5,9,5]) == [2,4,1,7,6,1,7,6]

main = do
	dat <- getInput
	let res = manyPhases 100 dat
	putStrLn $ map (intToDigit.fromInteger) $ take 8 res
