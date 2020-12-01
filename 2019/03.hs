{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Data.Function
import Data.Map as Map
import Control.Exception
import Control.Monad
import Utils

data Direction = LeftDir | RightDir | UpDir | DownDir
type Step = (Direction, Integer)
type Point = (Integer, Integer, Integer)

readDir :: Char -> Direction
readDir 'L' = LeftDir
readDir 'R' = RightDir
readDir 'U' = UpDir
readDir 'D' = DownDir

readTrail :: String -> Step
readTrail (d:n) = (readDir d, read n)

readTrails :: String -> [Step]
readTrails s = Prelude.map readTrail $ Utils.split ',' s

takeStep :: Direction -> Point -> Point
takeStep LeftDir (x, y, l) = (x-1, y, l + 1)
takeStep RightDir (x, y, l) = (x+1, y, l + 1)
takeStep UpDir (x, y, l) = (x, y+1, l + 1)
takeStep DownDir (x, y, l) = (x, y-1, l + 1)

followTrail :: [Step] -> [Point]
followTrail steps = iterfunc (0, 0, 0) steps
	where
		iterfunc p [] = []
		iterfunc p ((_, 0):rest) = iterfunc p rest
		iterfunc p ((d, n):rest) = let p' = takeStep d p in p' : iterfunc p' ((d, n-1):rest)

intersections :: [Point] -> [Point] -> [Point]
{-
intersections as bs = do
	(ax, ay, al) <- as
	(bx, by, bl) <- bs
	guard $ ax == bx
	guard $ ay == by
	return (ax, ay, al + bl)
-}
intersections as bs = res
	where
		liftval (x, y, l) = ((x, y), l)
		unliftval ((x, y), l) = (x, y, l)
		amap = fromList $ Prelude.map liftval as
		bmap = fromList $ Prelude.map liftval bs
		ints = intersectionWith (+) amap bmap
		res = Prelude.map unliftval $ assocs ints

manhattanSize :: Point -> Integer
manhattanSize (x, y, _) = abs x + abs y

wireLength :: Point -> Integer
wireLength (_, _, l) = l

closestIntersection :: [Point] -> [Point] -> Point
closestIntersection a b = minimumBy (compare `on` manhattanSize) $ intersections a b

shortestIntersection :: [Point] -> [Point] -> Point
shortestIntersection a b = minimumBy (compare `on` wireLength) $ intersections a b

tests :: IO ()
tests = do
	check $ followTrail (readTrails "R8,U5,L5,D3") == [(1,0,1), (2,0,2), (3,0,3), (4,0,4), (5,0,5), (6,0,6), (7,0,7), (8,0,8), (8,1,9), (8,2,10), (8,3,11), (8,4,12), (8,5,13), (7,5,14), (6,5,15), (5,5,16), (4,5,17), (3,5,18), (3,4,19), (3,3,20), (3,2,21)]

	check $ closestIntersection (followTrail $ readTrails "R8,U5,L5,D3") (followTrail $ readTrails "U7,R6,D4,L4") == (3, 3, 40)
	check $ manhattanSize (closestIntersection (followTrail $ readTrails "R75,D30,R83,U83,L12,D49,R71,U7,L72") (followTrail $ readTrails "U62,R66,U55,R34,D71,R55,D58,R83")) == 159
	check $ manhattanSize (closestIntersection (followTrail $ readTrails "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51") (followTrail $ readTrails "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")) == 135

	check $ shortestIntersection (followTrail $ readTrails "R8,U5,L5,D3") (followTrail $ readTrails "U7,R6,D4,L4") == (6, 5, 30)
	check $ wireLength (shortestIntersection (followTrail $ readTrails "R75,D30,R83,U83,L12,D49,R71,U7,L72") (followTrail $ readTrails "U62,R66,U55,R34,D71,R55,D58,R83")) == 610
	check $ wireLength (shortestIntersection (followTrail $ readTrails "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51") (followTrail $ readTrails "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")) == 410

	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

readInput :: IO ([Step], [Step])
readInput = do
	dat <- readFile "03.txt"
	let l1:l2:_ = lines dat
	return (readTrails l1, readTrails l2)

main = do
	(t1, t2) <- readInput
	let i = closestIntersection (followTrail t1) (followTrail t2)
	print i
	print $ manhattanSize i
	let i2 = shortestIntersection (followTrail t1) (followTrail t2)
	print i2
	print $ wireLength i2
