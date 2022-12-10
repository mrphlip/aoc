{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Control.Exception
import Utils

data Operation = NoOp | AddX Integer deriving (Eq, Ord, Show, Read)

getInput :: IO [Operation]
getInput = map (readLine.words) <$> lines <$> readFile "10.txt"

readLine :: [String] -> Operation
readLine ["noop"] = NoOp
readLine ["addx", x] = AddX $ read x

runOps :: [Operation] -> [Integer]
runOps ops = concat $ map snd $ scanl runOp (1, []) ops
	where
		runOp (x, _) NoOp = (x, [x])
		runOp (x, _) (AddX dx) = (x + dx, [x, x])

signal :: [Integer] -> Integer
signal xs = sum [i * (xs `genericIndex` (i-1)) | i <- [20,60..220]]

draw :: [Integer] -> [String]
draw xs = map drawRow $ chunk 40 xs
	where drawRow row = [if i >= x - 1 && i <= x + 1 then '#' else ' ' | (i, x) <- zip [0..] row]

tests :: IO ()
tests = do
	check $ signal res == 13140
	check $ draw res == ["##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ","###   ###   ###   ###   ###   ###   ### ","####    ####    ####    ####    ####    ","#####     #####     #####     #####     ","######      ######      ######      ####","#######       #######       #######     "]
	where
		testData = map (readLine.words) $ ["addx 15","addx -11","addx 6","addx -3","addx 5","addx -1","addx -8","addx 13","addx 4","noop","addx -1","addx 5","addx -1","addx 5","addx -1","addx 5","addx -1","addx 5","addx -1","addx -35","addx 1","addx 24","addx -19","addx 1","addx 16","addx -11","noop","noop","addx 21","addx -15","noop","noop","addx -3","addx 9","addx 1","addx -3","addx 8","addx 1","addx 5","noop","noop","noop","noop","noop","addx -36","noop","addx 1","addx 7","noop","noop","noop","addx 2","addx 6","noop","noop","noop","noop","noop","addx 1","noop","noop","addx 7","addx 1","noop","addx -13","addx 13","addx 7","noop","addx 1","addx -33","noop","noop","noop","addx 2","noop","noop","noop","addx 8","noop","addx -1","addx 2","addx 1","noop","addx 17","addx -9","addx 1","addx 1","addx -3","addx 11","noop","noop","addx 1","noop","addx 1","noop","noop","addx -13","addx -19","addx 1","addx 3","addx 26","addx -30","addx 12","addx -1","addx 3","addx 1","noop","noop","noop","addx -9","addx 18","addx 1","addx 2","noop","noop","addx 9","noop","noop","noop","addx -1","addx 2","addx -37","addx 1","addx 3","noop","addx 15","addx -21","addx 22","addx -6","addx 1","noop","addx 2","addx 1","noop","addx -10","noop","noop","addx 20","addx 1","addx 2","addx 2","addx -6","addx -11","noop","noop","noop"]
		res = runOps testData
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	let res = runOps dat
	print $ signal res
	mapM_ putStrLn $ draw res
