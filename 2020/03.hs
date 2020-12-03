{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Control.Exception

getInput :: IO [[Bool]]
getInput = do
	dat <- readFile "03.txt"
	return $ parseInput dat

parseInput :: String -> [[Bool]]
parseInput = map (map (=='#')) . lines

tracePath :: Integer -> Integer -> [[Bool]] -> [Bool]
tracePath dx dy trees = [trees `genericIndex` (i * dy) `genericIndex` ((i * dx) `mod` cx) | i <- [0..(cy-1) `div` dy]]
	where cy = genericLength trees; cx = genericLength $ head trees

countPath :: [Bool] -> Integer
countPath = genericLength . filter id

doCount :: Integer -> Integer -> [[Bool]] -> Integer
doCount dx dy trees = countPath $ tracePath dx dy trees

tests :: IO ()
tests = do
	check $ doCount 1 1 trees == 2
	check $ doCount 3 1 trees == 7
	check $ doCount 5 1 trees == 3
	check $ doCount 7 1 trees == 4
	check $ doCount 1 2 trees == 2
	where
		trees = parseInput "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	trees <- getInput
	print $ doCount 3 1 trees
	print $ doCount 1 1 trees * doCount 3 1 trees * doCount 5 1 trees * doCount 7 1 trees * doCount 1 2 trees
