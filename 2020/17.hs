{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Ix
import Data.List
import Data.List.Split (splitOn)
import Control.Exception
import Utils

type Point3 = (Integer, Integer, Integer)
type Point4 = (Integer, Integer, Integer, Integer)
type Grid i = Array i Bool

class (ExpandIx i) => LifeGrid i where
	readGrid :: String -> Grid i
	showGrid :: Grid i -> String
	expandByOne :: (i, i) -> (i, i)
	neighbours :: i -> [i]

getInput :: (LifeGrid i) => IO (Grid i)
getInput = do
	dat <- readFile "17.txt"
	return $ readGrid dat

instance (Num a, Enum a, ExpandIx a, Num b, Enum b, ExpandIx b, Num c, Enum c, ExpandIx c) => LifeGrid (a, b, c) where
	readGrid dat = array bounds cells
		where
			rows = lines dat
			cells = [ ((x,y,0), cell == '#') | (y,row) <- enumerate rows, (x,cell) <- enumerate row ]

			numrows = genericLength $ rows
			numcols = genericLength $ head $ rows
			bounds = ((0, 0, 0), (numcols - 1, numrows - 1, 0))
	showGrid grid = result
		where
			((minx, miny, minz), (maxx, maxy, maxz)) = bounds grid
			cell x y z = if grid ! (x,y,z) then '#' else '.'
			row y z = [ cell x y z | x <- [minx..maxx] ]
			level z = unlines [ row y z | y <- [miny..maxy] ]
			result = intercalate "\n\n" [ level z | z <- [minz..maxz] ]
	expandByOne ((minx, miny, minz), (maxx, maxy, maxz)) = ((minx-1, miny-1, minz-1), (maxx+1, maxy+1, maxz+1))
	neighbours (x, y, z) = [(x+dx, y+dy, z+dz) | dx<-[-1,0,1], dy<-[-1,0,1], dz<-[-1,0,1], dx/=0 || dy/=0 || dz/=0]

instance (Num a, Enum a, ExpandIx a, Num b, Enum b, ExpandIx b, Num c, Enum c, ExpandIx c, Num d, Enum d, ExpandIx d) => LifeGrid (a, b, c, d) where
	readGrid dat = array bounds cells
		where
			rows = lines dat
			cells = [ ((x,y,0,0), cell == '#') | (y,row) <- enumerate rows, (x,cell) <- enumerate row ]

			numrows = genericLength $ rows
			numcols = genericLength $ head $ rows
			bounds = ((0, 0, 0, 0), (numcols - 1, numrows - 1, 0, 0))
	showGrid grid = result
		where
			((minw, minx, miny, minz), (maxw, maxx, maxy, maxz)) = bounds grid
			cell w x y z = if grid ! (w,x,y,z) then '#' else '.'
			row x y z = [ cell w x y z | w <- [minw..maxw] ]
			level y z = unlines [ row x y z | x <- [minx..maxx] ]
			hyper z = intercalate "\n\n" [ level y z | y <- [miny..maxy] ]
			result = intercalate "\n\n=====\n\n\n" [ hyper z | z <- [minz..maxz] ]
	expandByOne ((minw, minx, miny, minz), (maxw, maxx, maxy, maxz)) = ((minw-1, minx-1, miny-1, minz-1), (maxw+1, maxx+1, maxy+1, maxz+1))
	neighbours (w, x, y, z) = [(w+dw, x+dx, y+dy, z+dz) | dw<-[-1,0,1], dx<-[-1,0,1], dy<-[-1,0,1], dz<-[-1,0,1], dw/=0 || dx/=0 || dy/=0 || dz/=0]

iterGrid :: (LifeGrid i) => Grid i -> Grid i
iterGrid grid = shrinkBounds $ listArray newbounds $ map calcCell $ range newbounds
	where
		newbounds = expandByOne $ bounds grid
		getCell p = getExpand p False grid
		calcCell p = neighbourcount == 3 || (neighbourcount == 2 && selfAlive)
			where
				selfAlive = getCell p
				neighbourcount = length $ filter getCell $ neighbours p

shrinkBounds :: (ExpandIx i) => Grid i -> Grid i
shrinkBounds grid = changeBounds newbounds False grid
	where
		cells = map fst $ filter snd $ assocs grid
		newbounds = foldl expandBounds (head cells, head cells) cells

countCells :: Grid i -> Integer
countCells = genericLength . filter id . elems

tests :: IO ()
tests = do
	check $ (countCells $ iterate iterGrid gliderA !! 6) == 112
	check $ (countCells $ iterate iterGrid gliderB !! 6) == 848
	where
		gliderA = readGrid ".#.\n..#\n###" :: Grid Point3
		gliderB = readGrid ".#.\n..#\n###" :: Grid Point4
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	initgridA <- getInput :: IO (Grid Point3)
	print $ countCells $ iterate iterGrid initgridA !! 6
	initgridB <- getInput :: IO (Grid Point4)
	print $ countCells $ iterate iterGrid initgridB !! 6
