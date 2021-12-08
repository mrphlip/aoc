{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Control.Exception
import Control.Monad
import Utils

type Board = [[Integer]]

getInput :: IO ([Integer], [Board])
getInput = do
	dat <- readFile "04.txt"
	return $ parseInput dat
parseInput :: String -> ([Integer], [Board])
parseInput dat = (calls, boards)
	where
		(callstxt:boardstxt) = splitOn "\n\n" dat
		calls = map read $ split ',' callstxt
		boards = map (map (map read . words) . lines) $ boardstxt

bingoLines :: Board -> [S.Set Integer]
bingoLines board = rows ++ cols {- ++ diags -}
	where
		rows = map S.fromList board
		cols = map S.fromList $ transpose board
		diags = [S.fromList [board !! i !! i | i <- [0..4]], S.fromList [board !! i !! (4-i) | i <- [0..4]]]

winTime :: [Integer] -> Board -> Integer
winTime calls board = genericLength $ takeWhile (not.isWin) callIter
	where
		callIter = scanl (flip S.insert) S.empty calls
		lines = bingoLines board
		isWin callset = any (`S.isSubsetOf` callset) lines

winSort :: [Integer] -> [Board] -> [(Board, Integer)]
winSort calls boards = sortOn snd [(board, winTime calls board) | board <- boards]

score :: [Integer] -> Board -> Integer -> Integer
score calls board wintime = sum (S.toList uncalled) * lastcall
	where
		boardset = S.fromList $ join board
		callset = S.fromList $ genericTake wintime calls
		uncalled = boardset `S.difference` callset
		lastcall = calls `genericIndex` (wintime - 1)

doPuzzle :: [Integer] -> [Board] -> (Integer, Integer)
doPuzzle calls boards = (partA, partB)
	where
		winorder = winSort calls boards
		(firstboard, firstwin) = head winorder
		(lastboard, lastwin) = last winorder
		partA = score calls firstboard firstwin
		partB = score calls lastboard lastwin

tests :: IO ()
tests = do
	check $ partA == 4512
	check $ partB == 1924
	where
		(calls, boards) = parseInput "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7"
		(partA, partB) = doPuzzle calls boards
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	(calls, boards) <- getInput
	let (partA, partB) = doPuzzle calls boards
	print partA
	print partB
