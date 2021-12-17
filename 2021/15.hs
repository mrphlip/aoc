{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Char
import Data.Ix
import Control.Exception
import Dijkstra
import Direction
import Utils

type Point = (Integer, Integer)
type Grid = Array Point Integer

getInput :: IO Grid
getInput = do
	dat <- readFile "15.txt"
	return $ parseInput dat
parseInput :: String -> Grid
parseInput = listArrayLen2 . map (map (toInteger.digitToInt)) . lines

solveGrid :: Grid -> Integer -> Integer
solveGrid grid multiple = distance
	where
		((0, 0), (xmax, ymax)) = bounds grid
		width = xmax + 1; height = ymax + 1
		startloc = (0,0)
		endloc = (width * multiple - 1, height * multiple - 1)
		expandedBounds = (startloc, endloc)

		neighbours p = [ (p', d, getWeight p') | d <- directions, let p' = step d p, inRange expandedBounds p' ]
		getWeight (x, y) = modulo $ grid ! (subx, suby) + repx + repy
			where (repx, subx) = x `divMod` width; (repy, suby) = y `divMod` height
		modulo n = (n - 1) `mod` 9 + 1

		Just (_, distance) = findNearest expandedBounds startloc neighbours (==endloc)

tests :: IO ()
tests = do
	check $ (solveGrid sample 1) == 40
	check $ (solveGrid sample 5) == 315
	where
		sample = parseInput "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	grid <- getInput
	print $ solveGrid grid 1
	print $ solveGrid grid 5
