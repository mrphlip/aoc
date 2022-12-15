{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P
import Control.Exception
import Utils

type Point = (Integer, Integer)
type Grid = S.Set Point

getInput :: IO [[Point]]
getInput = map parseLine <$> lines <$> readFile "14.txt"

parseLine :: String -> [Point]
parseLine = runReadP readPath
	where
		readInt = P.readS_to_P reads :: P.ReadP Integer
		readPoint = do
			a <- readInt
			P.char ','
			b <- readInt
			return (a, b)
		readPath = P.sepBy1 readPoint (P.string " -> ")

drawLines :: [[Point]] -> Grid
drawLines = S.fromList . concat . map drawLine
drawLine :: [Point] -> [Point]
drawLine ps = concat $ map (uncurry drawEdge) $ zip ps (tail ps)
drawEdge :: Point -> Point -> [Point]
drawEdge (x1, y1) (x2, y2)
	|	x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
	|	y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]

findFloor :: Grid -> Integer
findFloor g = (+1) $ maximum $ map snd $ S.elems g

-- dropSand grid floorHeight startpoint -> (endpoint, did it stop before reaching the floor?)
dropSand :: Grid -> Integer -> Point -> (Point, Bool)
dropSand grid floor (x, y)
	| y >= floor = ((x, y), False)
	| not $ (x, y + 1) `S.member` grid = dropSand grid floor (x, y + 1)
	| not $ (x - 1, y + 1) `S.member` grid = dropSand grid floor (x - 1, y + 1)
	| not $ (x + 1, y + 1) `S.member` grid = dropSand grid floor (x + 1, y + 1)
	| otherwise = ((x, y), True)

pourSand :: Grid -> [(Grid, Bool)]
pourSand grid = iterate stepFunc (grid, True)
	where
		floor = findFloor grid
		stepFunc (g, _) = let (p, stop) = dropSand g floor (500, 0) in (S.insert p g, stop)

partA :: Grid -> Integer
partA grid = subtract 1 $ genericLength $ takeWhile snd $ pourSand grid
partB :: Grid -> Integer
partB grid = genericLength $ takeWhile (not . S.member (500,0) . fst) $ pourSand grid

tests :: IO ()
tests = do
	check $ partA testGrid == 24
	check $ partB testGrid == 93
	where
		testData = map parseLine ["498,4 -> 498,6 -> 496,6", "503,4 -> 502,4 -> 502,9 -> 494,9"]
		testGrid = drawLines testData
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	let grid = drawLines dat
	print $ partA grid
	print $ partB grid
