{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.List
import Data.Maybe
import Control.Exception
import Utils
import Direction
import Dijkstra

type Point = (Integer, Integer)
type Grid = Array Point Integer
type Solution = DistMap Point Integer Direction

getInput :: IO (Grid, Point, Point)
getInput = parseInput <$> readFile "12.txt"

parseInput :: String -> (Grid, Point, Point)
parseInput s = (grid, startloc, endloc)
	where
		l = lines s
		asHeight 'S' = 0
		asHeight 'E' = 25
		asHeight c | 'a' <= c && c <= 'z' = toInteger $ fromEnum c - fromEnum 'a'
		grid = listArrayLen2 $ map (map asHeight) l
		findloc target = head [(x, y) | (y, row) <- zip [0..] l, (x, c) <- zip [0..] row, c == target]
		startloc = findloc 'S'
		endloc = findloc 'E'

solveMaze :: Grid -> Point -> Solution
solveMaze grid endloc = fst $ buildDistMap mapbounds endloc neighbours (const False)
	where
		mapbounds = bounds grid
		neighbours p = [(p', d, 1) | d <- directions, let p' = step d p, inRange mapbounds p', (grid ! p') >= (grid ! p) - 1]

partA :: Solution -> Point -> Integer
partA s p = n
	where Just (n, _, _) = s ! p
partB :: Grid -> Solution -> Integer
partB g s = minimum [n | p <- indices g, g ! p == 0, (n, _, _) <- maybeToList (s ! p)]

tests :: IO ()
tests = do
	check $ partA solution startloc == 31
	check $ partB grid solution == 29
	where
		(grid, startloc, endloc) = parseInput "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"
		solution = solveMaze grid endloc
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	(grid, startloc, endloc) <- getInput
	let solution = solveMaze grid endloc
	print $ partA solution startloc
	print $ partB grid solution
