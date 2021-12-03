{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Bits
import Data.List
import Control.Exception
import Utils

getInput :: IO (Int, [Integer])
getInput = do
	dat <- readFile "03.txt"
	return $ parseInput dat
parseInput :: String -> (Int, [Integer])
parseInput dat = if all ((==bitlen).length) ls then (bitlen, map (readBaseN 2) ls) else error "not all strings are the same length"
	where
		ls = lines dat
		bitlen = length $ head ls

mostCommon :: Int -> [Integer] -> Bool
mostCommon pos xs = countSet * 2 >= countAll
	where
		countAll = genericLength xs
		countSet = genericLength $ filter (flip testBit pos) xs

mostCommons :: Int -> [Integer] -> Integer
mostCommons bitlen xs = foldl1 (.|.) [bit i | i<-[0..bitlen-1], mostCommon i xs]

doSelect :: Bool -> Int -> [Integer] -> Integer
doSelect invert bitlen xs = worker (bitlen - 1) xs
	where
		worker :: Int -> [Integer] -> Integer
		worker _ [] = error "No result found"
		worker _ [x] = x
		worker pos xs = worker (pos - 1) $ filter ((==sel).flip testBit pos) xs
			where
				sel = mostCommon pos xs `xor` invert

partA bitlen xs = (gamma, epsilon)
	where
		gamma = mostCommons bitlen xs
		epsilon = complement gamma .&. (bit bitlen - 1)

partB bitlen xs = (o2, co2)
	where
		o2 = doSelect False bitlen xs
		co2 = doSelect True bitlen xs

tests :: IO ()
tests = do
	check $ partA bitlen values == (22, 9)
	check $ partB bitlen values == (23, 10)
	where
		(bitlen, values) = parseInput "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	(bitlen, values) <- getInput
	print $ uncurry (*) $ partA bitlen values
	print $ uncurry (*) $ partB bitlen values
