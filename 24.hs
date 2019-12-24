{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Ix
import Data.List
import Data.Bits
import qualified Data.Set as S
import Control.Exception
import Utils
import Direction

type Point = (Integer, Integer)
type Point3 = (Integer, Integer, Integer)
type Grid i = Array i Bool

getInput :: IO (Grid Point)
getInput = do
	dat <- readFile "24.txt"
	return $ parseInput dat

parseInput :: String -> Grid Point
parseInput s = array ((0,0),(width-1,height-1)) cells
	where
		rows = lines s
		height = genericLength rows
		width = genericLength $ head rows
		cells = [ ((y,x),cell c) | (y,row) <- enumerate rows, (x,c) <- enumerate row]
		cell = (=='#')

showGrid :: Grid Point -> String
showGrid grid = unlines rows
	where
		((minx,miny),(maxx,maxy)) = bounds grid
		rows = [ row y | y <- [miny..maxy] ]
		row y = [ cell x y | x <- [minx..maxx] ]
		cell x y
			|	grid ! (y,x) = '#'
			| otherwise = '.'

stepGrid :: (Ix i) => (i -> [i]) -> Grid i -> Grid i
stepGrid iterneighbours grid = array gridbounds nextcells
	where
		gridbounds = bounds grid
		nextcells = [ (p, nextcell p) | p <- indices grid ]
		nextcell p
			| grid ! p = neighbours == 1
			| otherwise = neighbours == 1 || neighbours == 2
			where neighbours = length $ [ () | p' <- iterneighbours p, getExpand p' False grid ]

iterneighboursA :: Point -> [Point]
iterneighboursA p = [ step d p | d <- directions ]
iterneighboursB :: Point3 -> [Point3]
iterneighboursB (2,2,_) = []
iterneighboursB (y,x,d) = direct ++ upout ++ downout ++ leftout ++ rightout ++ upin ++ downin ++ leftin ++ rightin
	where
		direct = [ (y',x',d) | dir <- directions, let (y',x') = step dir (y,x), x' >= 0, y' >= 0, x' < 5, y' < 5, (x',y') /= (2,2)]
		upout
			|	y == 0 = [(1,2,d-1)]
			| otherwise = []
		downout
			|	y == 4 = [(3,2,d-1)]
			| otherwise = []
		leftout
			|	x == 0 = [(2,1,d-1)]
			| otherwise = []
		rightout
			|	x == 4 = [(2,3,d-1)]
			| otherwise = []
		upin
			| (y,x) == (1,2) = [(0,x,d+1) | x <- [0..4]]
			| otherwise = []
		downin
			| (y,x) == (3,2) = [(4,x,d+1) | x <- [0..4]]
			| otherwise = []
		leftin
			| (y,x) == (2,1) = [(y,0,d+1) | y <- [0..4]]
			| otherwise = []
		rightin
			| (y,x) == (2,3) = [(y,4,d+1) | y <- [0..4]]
			| otherwise = []

stepGridA :: Grid Point -> Grid Point
stepGridA = stepGrid iterneighboursA
stepGridB :: Grid Point3 -> Grid Point3
stepGridB = stepGrid iterneighboursB

toBitset :: Grid Point -> Integer
toBitset grid = foldr1 (.|.) [ bit n | (n, ix) <- enumerate $ indices grid, grid ! ix ]

findRepeat :: Grid Point -> Grid Point
findRepeat initgrid = iterfunc initgrid S.empty
	where
		iterfunc :: Grid Point -> S.Set Integer -> Grid Point
		iterfunc grid seen
			|	bitset `S.member` seen = grid
			| otherwise = iterfunc (stepGridA grid) (bitset `S.insert` seen)
			where bitset = toBitset grid

liftGrid :: Integer -> Grid Point -> Grid Point3
liftGrid depth grid = array newbounds newdata
	where
		((miny,minx),(maxy,maxx)) = bounds grid
		newbounds = ((miny,minx,-depth),(maxy,maxx,depth))
		newdata = [ ((y,x,0),c) | ((y,x),c) <- assocs grid ] ++ [ ((y,x,d),False) | (y,x,d) <- range newbounds, d /= 0 ]

sliceGrid :: Integer -> Grid Point3 -> Grid Point
sliceGrid level grid = array newbounds newdata
	where
		((miny,minx,_),(maxy,maxx,_)) = bounds grid
		newbounds = ((miny,minx),(maxy,maxx))
		newdata = [ ((y,x),c) | ((y,x,d),c) <- assocs grid, d == level ]

countBugs :: (Ix i) => Grid i -> Integer
countBugs grid = genericLength $ filter id $ elems grid

partB :: Grid Point -> Integer -> Integer
partB grid steps = countBugs resultgrid
	where
		depth = steps `ceildiv` 2
		ceildiv p q
			| p `mod` q == 0 = p `div` q
			| otherwise = p `div` q + 1
		lifted = liftGrid depth grid
		resultgrid = (iterate stepGridB lifted) `genericIndex` steps

tests :: IO ()
tests = do
	let sample1 = parseInput "....#\n#..#.\n#..##\n..#..\n#...."
 	test $ (toBitset $ findRepeat sample1) == 2129920
	test $ partB sample1 10 == 99

main :: IO ()
main = do
	grid <- getInput
	print $ toBitset $ findRepeat grid
	print $ partB grid 200
