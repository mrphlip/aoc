{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Ix
import Data.List
import Data.Maybe
import Control.Exception
import Intcode
import Utils
import Direction
import Dijkstra
import Debug.Trace

data Cell = Unknown | Floor | Wall | Target deriving (Eq, Show, Read)
type Point = (Integer, Integer)
type Maze = Array Point Cell
type State = (Maze, Point)

getInput :: IO (IntcodeMem Integer)
getInput = do
	dat <- readFile "15.txt"
	return $ readProg dat

writeDirection UpDir = 1
writeDirection DownDir = 2
writeDirection LeftDir = 3
writeDirection RightDir = 4

makeInputs :: [Integer] -> ([Integer], Maze)
makeInputs outp = iterfunc (listArray ((0,0),(0,0)) [Floor], (0,0)) outp
	where
		iterfunc (maze, loc) outp = result
			where
				maybeRoute = findNearestSquareExpand maze loc [Floor, Target, Unknown] [Unknown] 1 Unknown
				route = fromJust maybeRoute
				(walkTo, testTo) = foldl (\(_, p) d -> (p, step d p)) (undefined,loc) route
				(walk, final:rest) = genericSplitAt (genericLength route - 1) outp
				(continue, finalstate) = case assert (all (/=0) walk) $ final of
					0 -> iterfunc (setExpand testTo Wall Unknown maze, walkTo) rest
					1 -> iterfunc (setExpand testTo Floor Unknown maze, testTo) rest
					2 -> iterfunc (setExpand testTo Target Unknown maze, testTo) rest
				result = if isNothing maybeRoute then ([], maze) else (map writeDirection route ++ continue, finalstate)
runprog :: IntcodeMem Integer -> Maze
runprog prog = maze
	where
		outputs = icrunOutp $ icinitInp prog inputs
		(inputs, maze) = makeInputs outputs

showMaze :: Maze -> String
showMaze maze = unlines $ rows
	where
		((minx, miny), (maxx, maxy)) = bounds maze
		rows = [ cells y | y <- [miny..maxy] ]
		cells y = [ cell x y | x <- [minx..maxx] ]
		cell 0 0 = '@'
		cell x y = showcell $ maze ! (x,y)
		showcell Unknown = '?'
		showcell Wall = '#'
		showcell Floor = '.'
		showcell Target = '*'

tests = do
	-- nb although this array is laid out horizontally here, it actually gets built vertically, Ix (x,y) is column-major
	let maze = listArray ((0,0),(2,9)) [
		Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,
		Target,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,
		Wall,Wall,Wall,Wall,Wall,Wall,Unknown,Wall,Wall,Wall]
	test $ findNearestSquareExpand maze (1,2) [Floor, Target, Unknown] [Unknown] 1 Unknown == Just [UpDir, UpDir, UpDir]
	test $ findNearestSquareExpand maze (1,4) [Floor, Target, Unknown] [Unknown] 1 Unknown == Just [DownDir, DownDir, RightDir]
	test $ findNearestSquareExpand maze (1,7) [Floor, Target, Unknown] [Unknown] 1 Unknown == Just [UpDir, RightDir]
	test $ findNearestSquareExpand maze (1,8) [Floor, Target, Unknown] [Unknown] 1 Unknown == Just [DownDir, DownDir]
	test $ findNearestSquareExpand maze (1,2) [Floor, Target] [Target] 1 Unknown == Just [UpDir, UpDir]
	test $ findNearestSquareExpand maze (1,4) [Floor, Target] [Target] 1 Unknown == Just [UpDir, UpDir, UpDir, UpDir]
	test $ findNearestSquareExpand maze (1,7) [Floor, Target] [Target] 1 Unknown == Just [UpDir, UpDir, UpDir, UpDir, UpDir, UpDir, UpDir]
	test $ findNearestSquareExpand maze (1,8) [Floor, Target] [Target] 1 Unknown == Just [UpDir, UpDir, UpDir, UpDir, UpDir, UpDir, UpDir, UpDir]

	let maze2 = listArray ((0,0),(2,2)) [Wall,Wall,Wall,Wall,Floor,Wall,Wall,Wall,Wall]
	test $ findNearestSquareExpand maze (1,1) [Floor, Target, Unknown] [Unknown] 1 Unknown == Nothing

main :: IO ()
main = do
	prog <- getInput
	let maze = runprog prog
	putStrLn $ showMaze maze
	let route = findNearestSquare maze (0,0) [Floor, Target] [Target]
	print route
	print $ length $ fromJust route

	let targetLoc = foldl (flip step) (0,0) $ fromJust route
	print targetLoc
	let (distmap, _) = buildDistMapSquare maze targetLoc [Floor, Target] []
	let maxDist = maximum $ map (\(x,_,_)->x) $ catMaybes $ elems distmap
	print maxDist
