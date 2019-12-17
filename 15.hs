{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Ix
import Data.List
import Data.Maybe
import Control.Exception
import Intcode
import Utils
import Direction
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

type DistMap = Array Point (Maybe (Integer, Direction))
buildDistMap :: Maybe Cell -> State -> (DistMap, Maybe Point)
buildDistMap maybeTarget (maze, startat) = iterfunc initdistmap
	-- buildDistMap Nothing state -> a full distance map of the entire maze
	-- buildDistMap (Just x) state -> an abbreviated distance map of the maze,
	--   enough to find the closest "x", plus the location of the closest "x"
	where
		allowed
			| isJust maybeTarget = [Floor,Target,fromJust maybeTarget]
			| isNothing maybeTarget = [Floor,Target]
		expbounds = let ((minx, miny), (maxx, maxy)) = bounds maze in ((minx-1, miny-1), (maxx+1, maxy+1))
		initdistmap :: DistMap
		initdistmap = listArray expbounds (repeat Nothing) // [ (startat, Just (0, UpDir)) ]
		-- Dijkstra-ish algorithm
		iterfunc :: DistMap -> (DistMap, Maybe Point)
		iterfunc distmap = if shouldStop then (distmap, Nothing) else continue
			where
				points :: [(Point, Direction, Integer)]
				points = [(step d p, d, fst $ fromJust $ distmap ! p) |
					p <- indices maze,
					d <- directions,
					isJust $ distmap ! p,
					isNothing $ distmap ! step d p,
					getExpand (step d p) Unknown maze `elem` allowed ]
				shouldStop = null points
				closestDist = minimum [ i | (_,_,i) <- points ]
				filtPoints = [ x | x@(_,_,i) <- points, i == closestDist ]
				newMap = distmap // [ (p, Just (closestDist + 1, d)) | (p,d,_) <- points ]
				targetPoints = [ p | x@(p,_,_) <- filtPoints, Just (getExpand p Unknown maze) == maybeTarget ]
				continue = case targetPoints of
					(p:_) -> (newMap, Just p)
					_ -> iterfunc newMap

findNearest :: Cell -> State -> Maybe [Direction]
findNearest target state = result
	where
		result = case buildDistMap (Just target) state of
			(finalMap, Just targetLoc) -> Just $ reverse $ tracePoint finalMap targetLoc
			(_, Nothing) -> Nothing
		tracePoint finalMap p
			| dist == 0 = []
			| otherwise = dir : tracePoint finalMap (step (reverseDirection dir) p)
			where Just (dist,dir) = finalMap ! p

makeInputs :: [Integer] -> ([Integer], Maze)
makeInputs outp = iterfunc (listArray ((0,0),(0,0)) [Floor], (0,0)) outp
	where
		iterfunc (maze, loc) outp = result
			where
				maybeRoute = findNearest Unknown (maze, loc)
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

	let targetLoc = foldl (flip step) (0,0) $ fromJust route
	print targetLoc
	let (distmap, _) = buildDistMap Nothing (maze, targetLoc)
	let maxDist = maximum $ map fst $ catMaybes $ elems distmap
	print maxDist
