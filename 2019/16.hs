{-# OPTIONS_GHC -Wno-tabs -threaded #-}
import Data.Char
import Data.List
import Data.Array
import Utils
import Debug.Trace

getInput :: IO [Integer]
getInput = do
	dat <- readFile "16.txt"
	return $ map (toInteger.digitToInt) $ takeWhile (/='\n') dat

alternating = cycle [-1, 1, 1, -1]

phase :: Integer -> [Integer] -> [Integer]
phase ofs xs = map getPos [ofs+1..len]
	where
		len = genericLength xs + ofs
		sums = 0 : zipWith (+) xs sums
		arrsums = listArray (ofs,len) sums
		getPos :: Integer -> Integer
		getPos n = (`mod` 10) $ abs $ sum $ zipWith (*) alternating $ map (arrsums!) $ indexes n
		indexes n = let res = [n-1,2*n-1..len] in if length res `mod` 2 == 1 then res ++ [len] else res

manyPhases :: Integer -> Integer -> [Integer] -> [Integer]
manyPhases ofs 0 xs = xs
manyPhases ofs n xs = let xs' = dotrace n $ phase ofs xs in sum xs' `seq` manyPhases ofs (n-1) xs'
	where
		--dotrace = traceShow
		dotrace _ = id

partA :: [Integer] -> [Integer]
partA dat = take 8 $ manyPhases 0 100 dat

partB :: [Integer] -> [Integer]
partB dat = take 8 $ manyPhases offset 100 $ genericDrop offset repeated
	where
		offset = read $ map (intToDigit.fromInteger) $ take 7 dat :: Integer
		repeated = genericTake (genericLength dat * 10000) $ cycle dat

tests :: IO ()
tests = do
	test $ phase 0 [1,2,3,4,5,6,7,8] == [4,8,2,2,6,1,5,8]
	test $ phase 0 [4,8,2,2,6,1,5,8] == [3,4,0,4,0,4,3,8]
	test $ phase 0 [3,4,0,4,0,4,3,8] == [0,3,4,1,5,5,1,8]
	test $ phase 0 [0,3,4,1,5,5,1,8] == [0,1,0,2,9,4,9,8]

	test $ partA [8,0,8,7,1,2,2,4,5,8,5,9,1,4,5,4,6,6,1,9,0,8,3,2,1,8,6,4,5,5,9,5] == [2,4,1,7,6,1,7,6]

	test $ partB [0,3,0,3,6,7,3,2,5,7,7,2,1,2,9,4,4,0,6,3,4,9,1,5,6,5,4,7,4,6,6,4] == [8,4,4,6,2,0,2,6]

main = do
	dat <- getInput
	let res = partA dat
	putStrLn $ map (intToDigit.fromInteger) $ res

	let res2 = partB dat
	putStrLn $ map (intToDigit.fromInteger) $ res2
