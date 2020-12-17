{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Ix
import Data.List
import Data.List.Split (splitOn)
import Control.Exception
import Utils

type Point = (Integer, Integer, Integer)
type Grid = Array Point Bool

getInput :: IO Grid
getInput = do
	dat <- readFile "17.txt"
	return $ readGrid dat

readGrid :: String -> Grid
readGrid dat = array bounds cells
	where
		rows = lines dat
		cells = [ ((x,y,0), cell == '#') | (y,row) <- enumerate rows, (x,cell) <- enumerate row ]

		numrows = genericLength $ rows
		numcols = genericLength $ head $ rows
		bounds = ((0, 0, 0), (numcols - 1, numrows - 1, 0))

showGrid :: Grid -> String
showGrid grid = result
	where
		((minx, miny, minz), (maxx, maxy, maxz)) = bounds grid
		cell x y z = if grid ! (x,y,z) then '#' else '.'
		row y z = [ cell x y z | x <- [minx..maxx] ]
		level z = unlines [ row y z | y <- [miny..maxy] ]
		result = intercalate "\n\n" [ level z | z <- [minz..maxz] ]

expandByOne :: (Point, Point) -> (Point, Point)
expandByOne ((minx, miny, minz), (maxx, maxy, maxz)) = ((minx-1, miny-1, minz-1), (maxx+1, maxy+1, maxz+1))

iterGrid :: Grid -> Grid
iterGrid grid = listArray newbounds $ map calcCell $ range newbounds
	where
		newbounds = expandByOne $ bounds grid
		getCell p = getExpand p False grid
		calcCell :: Point -> Bool
		calcCell (x, y, z) = neighbourcount == 3 || (neighbourcount == 2 && selfAlive)
			where
				selfAlive = getCell (x, y, z)
				neighbours = [(x+dx, y+dy, z+dz) | dx<-[-1,0,1], dy<-[-1,0,1], dz<-[-1,0,1], dx/=0 || dy/=0 || dz/=0]
				neighbourcount = length $ filter getCell neighbours

countCells :: Grid -> Integer
countCells = genericLength . filter id . elems

type PointB = (Integer, Integer, Integer, Integer)
type GridB = Array PointB Bool

getInputB :: IO GridB
getInputB = do
	dat <- readFile "17.txt"
	return $ readGridB dat

readGridB :: String -> GridB
readGridB dat = array bounds cells
	where
		rows = lines dat
		cells = [ ((x,y,0,0), cell == '#') | (y,row) <- enumerate rows, (x,cell) <- enumerate row ]

		numrows = genericLength $ rows
		numcols = genericLength $ head $ rows
		bounds = ((0, 0, 0, 0), (numcols - 1, numrows - 1, 0, 0))

showGridB :: GridB -> String
showGridB grid = result
	where
		((minw, minx, miny, minz), (maxw, maxx, maxy, maxz)) = bounds grid
		cell w x y z = if grid ! (w,x,y,z) then '#' else '.'
		row x y z = [ cell w x y z | w <- [minw..maxw] ]
		level y z = unlines [ row x y z | x <- [minx..maxx] ]
		hyper z = intercalate "\n\n" [ level y z | y <- [miny..maxy] ]
		result = intercalate "\n\n=====\n\n\n" [ hyper z | z <- [minz..maxz] ]


expandByOneB :: (PointB, PointB) -> (PointB, PointB)
expandByOneB ((minw, minx, miny, minz), (maxw, maxx, maxy, maxz)) = ((minw-1, minx-1, miny-1, minz-1), (maxw+1, maxx+1, maxy+1, maxz+1))

iterGridB :: GridB -> GridB
iterGridB grid = listArray newbounds $ map calcCell $ range newbounds
	where
		newbounds = expandByOneB $ bounds grid
		getCell p = getExpand p False grid
		calcCell :: PointB -> Bool
		calcCell (w, x, y, z) = neighbourcount == 3 || (neighbourcount == 2 && selfAlive)
			where
				selfAlive = getCell (w, x, y, z)
				neighbours = [(w+dw, x+dx, y+dy, z+dz) | dw<-[-1,0,1], dx<-[-1,0,1], dy<-[-1,0,1], dz<-[-1,0,1], dw/=0 || dx/=0 || dy/=0 || dz/=0]
				neighbourcount = length $ filter getCell neighbours

countCellsB :: GridB -> Integer
countCellsB = genericLength . filter id . elems

tests :: IO ()
tests = do
	check $ (countCells $ iterate iterGrid glider !! 6) == 112
	check $ (countCellsB $ iterate iterGridB gliderB !! 6) == 848
	where
		glider = readGrid ".#.\n..#\n###"
		gliderB = readGridB ".#.\n..#\n###"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	initgrid <- getInput
	print $ countCells $ iterate iterGrid initgrid !! 6
	initgridB <- getInputB
	print $ countCellsB $ iterate iterGridB initgridB !! 6
