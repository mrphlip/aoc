{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Ix
import Data.List
import Control.Exception
import Utils

type Point = (Integer, Integer)
type Grid = Array Point Bool
type GridState = (Grid, Bool)
type Rule = Array Integer Bool

getInput :: IO (Rule, GridState)
getInput = do
	dat <- readFile "20.txt"
	return $ parseInput dat
parseInput :: String -> (Rule, GridState)
parseInput dat = (parseRule firstline, (parseGrid rest, False))
	where (firstline:"":rest) = lines dat
parseRule :: String -> Rule
parseRule = listArrayLen . map (=='#')
parseGrid :: [String] -> Grid
parseGrid = listArrayLen2 . map (map (=='#'))

doStep :: Rule -> GridState -> GridState
doStep rule (grid, border) = (newgrid, newborder)
	where
		((minx,miny),(maxx,maxy)) = bounds grid
		get p
			|	inBounds grid p = grid ! p
			| otherwise = border
		newbounds = ((minx-1,miny-1),(maxx+1,maxy+1))
		newcell (x,y) = rule ! fromBaseN 2 [ if get (x+dx,y+dy) then 1 else 0 | dy<-[-1,0,1], dx<-[-1,0,1] ]
		newgrid = listArray newbounds $ map newcell $ range newbounds
		newborder = if border then rule!511 else rule!0

countLit :: GridState -> Integer
countLit (grid, True) = error "Count is infinite"
countLit (grid, False) = genericLength $ filter id $ elems grid

tests :: IO ()
tests = do
	check $ (countLit $ steps !! 2) == 35
	check $ (countLit $ steps !! 50) == 3351
	where
		(rule, state) = parseInput "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\n#..#.\n#....\n##..#\n..#..\n..###"
		steps = iterate (doStep rule) state
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	(rule, state) <- getInput
	let steps = iterate (doStep rule) state
	print $ countLit $ steps !! 2
	print $ countLit $ steps !! 50
