{-# OPTIONS_GHC -Wno-tabs #-}
import qualified Data.Set as S
import Control.Exception
import Utils

type Point = (Integer, Integer)
type Grid = S.Set Point
data FoldDir = FoldX | FoldY deriving Eq
type Fold = (FoldDir, Integer)

getInput :: IO (Grid, [Fold])
getInput = do
	dat <- readFile "13.txt"
	return $ parseInput dat
parseInput :: String -> (Grid, [Fold])
parseInput dat = (S.fromList $ map parsePoint points, map parseFold $ tail folds)
	where (points, folds) = break null $ lines dat
parsePoint :: String -> Point
parsePoint dat = (read x, read y)
	where [x,y] = split ',' dat
parseFold :: String -> Fold
parseFold ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':dir:'=':val) = (parseDir dir, read val)
	where
		parseDir 'x' = FoldX
		parseDir 'y' = FoldY

showGrid :: Grid -> String
showGrid grid = unlines $ rows
	where
		points = S.toList grid
		xmin = minimum $ map fst $ points
		xmax = maximum $ map fst $ points
		ymin = minimum $ map snd $ points
		ymax = maximum $ map snd $ points
		cell x y
			|	(x,y) `S.member` grid = '#'
			| otherwise = ' '
		row y = map (flip cell y) [xmin..xmax]
		rows = map row [ymin..ymax]
printGrid :: Grid -> IO ()
printGrid = putStrLn . showGrid

fold1D :: Integer -> Integer -> Integer
fold1D f x
	| x > f = 2*f - x
	| otherwise = x
fold2D :: Fold -> Point -> Point
fold2D (FoldX, f) (x,y) = (fold1D f x, y)
fold2D (FoldY, f) (x,y) = (x, fold1D f y)

foldGrid :: Fold -> Grid -> Grid
foldGrid f grid = S.map (fold2D f) grid

doFolds :: [Fold] -> Grid -> Grid
doFolds folds grid = foldl (flip foldGrid) grid folds

tests :: IO ()
tests = do
	check $ (S.size $ foldGrid (head folds) grid) == 17
	check $ doFolds folds grid == square
	where
		(grid, folds) = parseInput "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5"
		square = S.fromList $ [(i,0)|i<-[0..4]] ++ [(i,4)|i<-[0..4]] ++ [(0,i)|i<-[0..4]] ++ [(4,i)|i<-[0..4]]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	(grid, folds) <- getInput
	print $ S.size $ foldGrid (head folds) grid
	printGrid $ doFolds folds grid
