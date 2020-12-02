{-# OPTIONS_GHC -Wno-tabs #-}
import qualified Data.Set as S
import Control.Exception

getInput :: IO (S.Set Integer)
getInput = do
	dat <- readFile "01.txt"
	return $ S.fromList $ map read $ lines dat

findTwoSum :: Integer -> S.Set Integer -> [Integer]
findTwoSum total values = [a * b |
	a <- S.toList values,
	let b = total - a,
	b `S.member` values]

findThreeSum :: Integer -> S.Set Integer -> [Integer]
findThreeSum total values = [a * b * c |
	a <- S.toList values,
	b <- S.toList values,
	let c = total - a - b,
	c `S.member` values]

tests :: IO ()
tests = do
	check $ head (findTwoSum 2020 values) == 514579
	check $ head (findThreeSum 2020 values) == 241861950
	where
		values = S.fromList [1721, 979, 366, 299, 675, 1456]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	values <- getInput
	print $ head $ findTwoSum 2020 values
	print $ head $ findThreeSum 2020 values
