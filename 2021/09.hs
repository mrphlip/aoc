{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Char
import Data.List
import qualified Data.Set as S
import Control.Exception
import Utils

type Point = (Integer, Integer)
type Bounds = (Point, Point)
type Grid = Array Point Int

getInput :: IO Grid
getInput = do
	dat <- readFile "09.txt"
	return $ parseInput dat
parseInput = listArrayLen2 . map (map digitToInt) . lines

neighbours :: Bounds -> Point -> [Point]
neighbours bounds (x,y) = filter (inRange bounds) $ [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

localMinima :: Grid -> [Point]
localMinima grid = filter isMinima $ indices grid
	where isMinima p = all ((>grid!p).(grid!)) $ neighbours (bounds grid) p

partA :: Grid -> Integer
partA grid = sum $ map ((+1).toInteger.(grid!)) $ localMinima grid

findBasin :: Grid -> Point -> [Point]
findBasin grid p0 = worker S.empty [p0]
	where
		worker points [] = S.toList points
		worker points (p:rest)
			| p `S.member` points = worker points rest
			| otherwise = worker (S.insert p points) (rest ++ next)
				where next = filter ((/=9).(grid!)) $ filter ((>grid!p).(grid!)) $ neighbours (bounds grid) p

partB :: Grid -> Integer
partB grid = foldl1 (*) $ take 3 $ reverse $ sort basinSize
	where basinSize = map (genericLength . findBasin grid) $ localMinima grid

tests :: IO ()
tests = do
	check $ (partA grid) == 15
	check $ (partB grid) == 1134
	where
		grid = parseInput "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	grid <- getInput
	print $ partA grid
	print $ partB grid
