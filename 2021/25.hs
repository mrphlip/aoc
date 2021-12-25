{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import qualified Data.Map.Strict as M
import Control.Exception
import Direction
import Utils

type Point = (Integer, Integer)
type Size = (Integer, Integer)
type State = M.Map Point Direction

getInput :: IO (State, Size)
getInput = do
	dat <- readFile "25.txt"
	return $ parseInput dat
parseInput :: String -> (State, Point)
parseInput dat = (state, (width, height))
	where
		rows = lines dat
		state = M.fromList [ ((x,y),case c of '>' -> RightDir; 'v' -> DownDir) | (y,row) <- zip [0..] rows, (x, c) <- zip [0..] row, c /= '.' ]
		height = genericLength rows
		width = genericLength $ head rows

wrap :: Size -> Point -> Point
wrap (width, height) (x, y) = (x `mod` width, y `mod` height)

stepwrap :: Size -> Direction -> Point -> Point
stepwrap size dir p = wrap size $ step dir p

halfIteration :: Size -> Direction -> State -> (State, Bool)
halfIteration size dir state = (state `M.union` toAdd `M.difference` toRemove, null toMove)
	where
		toMove = [ (p, p') | (p,d) <- M.assocs state, d == dir, let p' = stepwrap size dir p, not $ M.member p' state ]
		toRemove = M.fromList [ (p, dir) | (p, _) <- toMove ]
		toAdd = M.fromList [ (p', dir) | (_, p') <- toMove ]

fullIteration :: Size -> State -> Maybe State
fullIteration size state = if noneRight && noneDown then Nothing else Just state''
	where
		(state', noneRight) = halfIteration size RightDir state
		(state'', noneDown) = halfIteration size DownDir state'

countIterations :: Size -> State -> Integer
countIterations size = (+1) . genericLength . unfoldr1 (fullIteration size)

showGrid :: Size -> State -> String
showGrid (width, height) state = unlines $ map row [0..height-1]
	where
		row y = map (cell y) [0..width-1]
		cell y x = case state M.!? (x,y) of
			Nothing -> '.'
			Just RightDir -> '>'
			Just DownDir -> 'v'

tests :: IO ()
tests = do
	check $ (countIterations size sample) == 58
	where
		(sample, size) = parseInput "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	(state, size) <- getInput
	print $ countIterations size state
