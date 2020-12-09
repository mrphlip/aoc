{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Exception
import Utils

getInput :: IO [Integer]
getInput = do
	dat <- readFile "09.txt"
	return $ map read $ lines dat

withHistory :: Integer -> [a] -> [([a], a)]
withHistory n xs = let (front, back) = genericSplitAt n xs in worker front back
	where
		worker _ [] = []
		worker front (x:rest) = (front, x) : worker (tail front ++ [x]) rest

findSum :: [Integer] -> Integer -> [(Integer, Integer)]
findSum vals total = [ (a, b) | a <- vals, let b = total - a, b > a, b `S.member` setvals ]
	where setvals = S.fromList vals

firstNonSum :: Integer -> [Integer] -> Integer
firstNonSum n xs = snd $ head $ dropWhile (not . null . uncurry findSum) $ withHistory n xs

findRangeSum :: [Integer] -> Integer -> [[Integer]]
findRangeSum xs total = [ take (b - a) $ drop a xs | (a, b) <- opts ]
	where
		cumulative = scanl (+) 0 xs
		valix = M.fromList [ (x, ix) | (ix, x) <- enumerate cumulative ]
		opts = [ (aix, bix) |
			a <- cumulative,
			let b = a + total,
			b `M.member` valix,
			let aix = valix M.! a,
			let bix = valix M.! b,
			bix >= aix + 2 ]

getResult :: [Integer] -> Integer
getResult xs = maximum xs + minimum xs

tests :: IO ()
tests = do
	check $ firstNonSum 5 testdat == 127
	check $ map getResult (findRangeSum testdat 127) == [62]
	where
		testdat = [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	dat <- getInput
	let partA = firstNonSum 25 dat
	print partA
	print $ map getResult $ findRangeSum dat partA
