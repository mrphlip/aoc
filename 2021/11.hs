{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Char
import Data.Ix
import Data.List
import qualified Data.Set as S
import Control.Exception
import Utils

type Point = (Integer, Integer)
type Grid = Array Point Int

getInput :: IO Grid
getInput = do
	dat <- readFile "11.txt"
	return $ parseInput dat
parseInput = listArrayLen2 . map (map digitToInt) . lines

neighbours :: (Point, Point) -> Point -> [Point]
neighbours bounds (x,y) = filter (inRange bounds) $ [(x+dx, y+dy) | dx<-[-1,0,1], dy<-[-1,0,1], dx/=0 || dy/=0]

doStep :: Grid -> (Integer, Grid)
doStep grid = checkFlashes S.empty $ increment grid
increment :: Grid -> Grid
increment grid = fmap (+1) grid
checkFlashes :: S.Set Point -> Grid -> (Integer, Grid)
checkFlashes flashes grid
	|	null newFlashes = (toInteger $ S.size flashes, grid // [(p,0) | p<-S.toList flashes])
	| otherwise = checkFlashes flashes' grid'
	where
		newFlashes = map fst $ filter ((>9).snd) $ filter (not.(`S.member` flashes).fst) $ assocs grid
		flashes' = S.union flashes $ S.fromList newFlashes
		grid' = accum (+) grid [(n, 1) | p <- newFlashes, n <- neighbours (bounds grid) p]

partA :: Integer -> Grid -> Integer
partA n grid = sum $ genericTake n $ unfoldr (Just . doStep) grid

partB :: Grid -> Integer
partB grid = (+1) $ genericLength $ takeWhile (not.(==target)) $ unfoldr (Just . doStep) grid
	where target = let ((xmin,ymin),(xmax,ymax)) = bounds grid in (xmax-xmin+1) * (ymax-ymin+1)

tests :: IO ()
tests = do
	check $ (partA 10 grid) == 204
	check $ (partA 100 grid) == 1656
	check $ partB grid == 195
	where
		grid = parseInput "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"
		showGrid g = let ((xmin,ymin),(xmax,ymax)) = bounds g in unlines [[intToDigit $ g!(x,y) | x<-[xmin..xmax]] | y<-[ymin..ymax]]

main :: IO ()
main = do
	tests
	grid <- getInput
	print $ partA 100 grid
	print $ partB grid
