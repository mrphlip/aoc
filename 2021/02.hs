{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Control.Exception

data Opcode = Forward | Down | Up deriving Eq
type Operation = (Opcode, Integer)

getInput :: IO [Operation]
getInput = do
	dat <- readFile "02.txt"
	return $ parseInput dat
parseInput :: String -> [Operation]
parseInput dat = do
	line <- lines dat
	let [opcode, amount] = words line
	return (parseOpcode opcode, read amount)
parseOpcode "forward" = Forward
parseOpcode "up" = Up
parseOpcode "down" = Down

partA :: [Operation] -> (Integer, Integer)
partA ops = (forwards, downs - ups)
	where
		forwards = sum $ map snd $ filter ((==Forward).fst) ops
		downs = sum $ map snd $ filter ((==Down).fst) ops
		ups = sum $ map snd $ filter ((==Up).fst) ops

partB :: [Operation] -> (Integer, Integer)
partB ops = fst $ foldl doOp ((0,0),0) ops
	where
		doOp ((x, y), aim) (Forward, n) = ((x + n, y + n*aim), aim)
		doOp ((x, y), aim) (Down, n) = ((x, y), aim + n)
		doOp ((x, y), aim) (Up, n) = ((x, y), aim - n)

tests :: IO ()
tests = do
	check $ partA sample == (15, 10)
	check $ partB sample == (15, 60)
	where
		sample = parseInput "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	print $ uncurry (*) $ partA dat
	print $ uncurry (*) $ partB dat
