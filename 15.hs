{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Ix
import Data.List
import Data.Maybe
import Control.Exception
import Intcode
import Utils
import Debug.Trace

data Cell = Unknown | Floor | Wall | Target deriving (Eq, Show, Read)
data Direction = LeftDir | RightDir | UpDir | DownDir deriving (Eq, Show, Read)
directions = [LeftDir, RightDir, UpDir, DownDir]
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
reverseDirection UpDir = DownDir
reverseDirection DownDir = UpDir
reverseDirection LeftDir = RightDir
reverseDirection RightDir = LeftDir

step :: Direction -> Point -> Point
step LeftDir (x, y) = (x-1, y)
step RightDir (x, y) = (x+1, y)
step UpDir (x, y) = (x, y-1)
step DownDir (x, y) = (x, y+1)

type DistMap = Array Point (Maybe (Integer, Direction))
findNearest :: Cell -> State -> Maybe [Direction]
findNearest target (maze, startat) = result
	where
		expbounds = let ((minx, miny), (maxx, maxy)) = bounds maze in ((minx-1, miny-1), (maxx+1, maxy+1))
		initdistmap :: DistMap
		initdistmap = listArray expbounds (repeat Nothing) // [ (startat, Just (0, UpDir)) ]
		-- Dijkstra-ish algorithm
		iterfunc :: DistMap -> Maybe (DistMap, Point)
		iterfunc distmap = if shouldStop then Nothing else continue
			where
				points :: [(Point, Direction, Integer)]
				points = [(step d p, d, fst $ fromJust $ distmap ! p) |
					p <- indices maze,
					d <- directions,
					isJust $ distmap ! p,
					isNothing $ distmap ! step d p,
					getExpand (step d p) Unknown maze `elem` [Floor,Target,target] ]
				shouldStop = null points
				closestDist = minimum [ i | (_,_,i) <- points ]
				filtPoints = [ x | x@(_,_,i) <- points, i == closestDist ]
				newMap = distmap // [ (p, Just (closestDist + 1, d)) | (p,d,_) <- points ]
				targetPoints = [ p | x@(p,_,_) <- filtPoints, getExpand p Unknown maze == target ]
				continue = case targetPoints of
					(p:_) -> Just (newMap, p)
					_ -> iterfunc newMap
		tracePoint finalMap p
			| dist == 0 = []
			| otherwise = dir : tracePoint finalMap (step (reverseDirection dir) p)
			where Just (dist,dir) = finalMap ! p
		result = case iterfunc initdistmap of
			Just (finalMap, targetLoc) -> Just $ reverse $ tracePoint finalMap targetLoc
			Nothing -> Nothing

makeInputs :: [Integer] -> ([Integer], Maze)
makeInputs outp = iterfunc (listArray ((0,0),(0,0)) [Floor], (0,0)) outp
	where
		iterfunc (maze, loc) outp = result
			where
				maybeRoute = findNearest Unknown (maze, loc)
				route = fromJust maybeRoute
				(walkTo, testTo) = foldl (\(_, p) d -> (p, step d p)) (undefined,loc) route
				(walk, final:rest) = genericSplitAt (genericLength route - 1) outp
				(continue, finalstate) = case assert (all (==1) walk) $ final of
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
	test $ findNearest Unknown (maze, (1,2)) == Just [UpDir, UpDir, UpDir]
	test $ findNearest Unknown (maze, (1,4)) == Just [DownDir, DownDir, RightDir]
	test $ findNearest Unknown (maze, (1,7)) == Just [UpDir, RightDir]
	test $ findNearest Unknown (maze, (1,8)) == Just [DownDir, DownDir]
	test $ findNearest Target (maze, (1,2)) == Just [UpDir, UpDir]
	test $ findNearest Target (maze, (1,4)) == Just [UpDir, UpDir, UpDir, UpDir]
	test $ findNearest Target (maze, (1,7)) == Just [UpDir, UpDir, UpDir, UpDir, UpDir, UpDir, UpDir]
	test $ findNearest Target (maze, (1,8)) == Just [UpDir, UpDir, UpDir, UpDir, UpDir, UpDir, UpDir, UpDir]

	let maze2 = listArray ((0,0),(2,2)) [Wall,Wall,Wall,Wall,Floor,Wall,Wall,Wall,Wall]
	test $ findNearest Unknown (maze, (1,1)) == Nothing

main :: IO ()
main = do
	prog <- getInput
	let maze = runprog prog
	putStrLn $ showMaze maze
	let route = findNearest Target (maze, (0,0))
	print route
	print $ length $ fromJust route
