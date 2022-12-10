{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Char
import Data.List
import Control.Exception
import Utils
import Direction

type Grid = Array (Integer,Integer) Int

getInput :: IO Grid
getInput = parseInput <$> readFile "08.txt"

parseInput :: String -> Grid
parseInput = listArrayLen2 . map (map digitToInt) . lines

visibility :: Grid -> (Integer, Integer) -> [(Integer, Bool)]
visibility g p = map checkVis directions
	where
		target = g ! p
		checkVis d = walk d (step d p) 0
		walk d p' n
			| not $ inBounds g p' = (n, True)
			| (g ! p') >= target = (n + 1, False)
			| otherwise = walk d (step d p') (n + 1)

partA :: Grid -> Integer
partA g = genericLength $ filter isVisible $ indices g
	where
		isVisible (x,y) = any snd $ visibility g (x,y)

partB :: Grid -> Integer
partB g = maximum $ map (product.map fst.visibility g) $ indices g

tests :: IO ()
tests = do
	check $ partA testData == 21
	check $ partB testData == 8
	where
		testData = parseInput "30373\n25512\n65332\n33549\n35390"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	print $ partA dat
	print $ partB dat
