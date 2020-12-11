{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Ix
import Data.List
import Control.Exception
import Utils

data Cell = Floor | Empty | Filled deriving (Eq, Show, Read)
type Point = (Integer, Integer)
type FloorPlan = Array Point Cell

getInput :: IO FloorPlan
getInput = do
	dat <- readFile "11.txt"
	return $ readMaze dat

readCell :: Char -> Cell
readCell '.' = Floor
readCell 'L' = Empty
readCell '#' = Filled

readMaze :: String -> FloorPlan
readMaze s = array bounds cells
	where
		width = genericLength $ head rows
		height = genericLength rows
		bounds = ((0, 0), (width-1, height-1))

		rows = lines s
		cells = [ ((x,y), readCell c) | (y, row) <- enumerate $ rows, (x, c) <- enumerate row ]

neighboursA :: FloorPlan -> Point -> [Point]
neighboursA plan (x, y) = [ (x + dx, y + dy) |
	dx <- [-1, 0, 1],
	dy <- [-1, 0, 1],
	dx /= 0 || dy /= 0,
	inRange mapbounds (x + dx, y + dy)]
	where
		mapbounds = bounds plan

neighboursB :: FloorPlan -> Point -> [Point]
neighboursB plan p = [ p' |
	dx <- [-1, 0, 1],
	dy <- [-1, 0, 1],
	dx /= 0 || dy /= 0,
	p' <- findNeighbour p dx dy]
	where
		mapbounds = bounds plan
		findNeighbour (x, y) dx dy
			| not $ inRange mapbounds p' = []
			| plan ! p' == Floor = findNeighbour p' dx dy
			| otherwise = [p']
			where p' = (x + dx, y + dy)

iterateFunc :: (FloorPlan -> Point -> [Point]) -> (Int -> Bool) -> (Int -> Bool) -> FloorPlan -> FloorPlan
iterateFunc neighbours becomeFilled becomeEmpty fromstate = listArray mapbounds $ map newState $ range mapbounds
	where
		mapbounds = bounds fromstate
		countNeighbours p = length $ filter (==Filled) $ map (fromstate!) $ neighbours fromstate p
		newState p = case fromstate ! p of
			Floor -> Floor
			Empty -> if becomeFilled $ countNeighbours p then Filled else Empty
			Filled -> if becomeEmpty $ countNeighbours p then Empty else Filled

iterateA :: FloorPlan -> FloorPlan
iterateA = iterateFunc neighboursA (==0) (>=4)
iterateB :: FloorPlan -> FloorPlan
iterateB = iterateFunc neighboursB (==0) (>=5)

findStable :: (Eq a) => (a -> a) -> a -> a
findStable f x
	| x == x' = x
	| otherwise = findStable f x'
	where x' = f x
findStableA :: FloorPlan -> FloorPlan
findStableA = findStable iterateA
findStableB :: FloorPlan -> FloorPlan
findStableB = findStable iterateB

countFilled :: FloorPlan -> Integer
countFilled = genericLength . filter (==Filled) . elems

tests = do
	check $ countFilled (findStableA testdat) == 37
	check $ countFilled (findStableB testdat) == 26
	where
		testdat = readMaze "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	plan <- getInput
	print $ countFilled $ findStableA plan
	print $ countFilled $ findStableB plan
