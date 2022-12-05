{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Control.Exception
import qualified Text.ParserCombinators.ReadP as P
import Utils

type Stacks = [[Char]]
type Move = (Integer, Integer, Integer)

getInput :: IO (Stacks, [Move])
getInput = do
	dat <- readFile "05.txt"
	return $ parseInput dat

parseInput :: String -> (Stacks, [Move])
parseInput dat = (parseStacks $ init stacks, map parseMove moves)
	where [stacks, moves] = splitOn [""] $ lines dat

parseStacks :: [String] -> Stacks
parseStacks dat = map catMaybes $ transpose parsed
	where
		readStacks = P.sepBy1 readEntry (P.char ' ')
		readEntry = readBlock P.+++ readSpace
		readBlock = Just <$> P.between (P.char '[') (P.char ']') P.get
		readSpace = Nothing <$ P.string "   "
		parsed = map (runReadP readStacks) dat

parseMove :: String -> Move
parseMove = runReadP readMove
	where
		readInt = P.readS_to_P reads :: P.ReadP Integer
		readMove = do
			P.string "move "
			count <- readInt
			P.string " from "
			from <- readInt
			P.string " to "
			to <- readInt
			return (count, from - 1, to - 1)

applyMove :: Bool -> Stacks -> Move -> Stacks
applyMove doReverse stacks (count, from, to) = newStacks
	where
		rev = if doReverse then reverse else id
		toMove = genericTake count $ stacks `genericIndex` from
		newFrom = genericDrop count $ stacks `genericIndex` from
		newTo = rev toMove ++ stacks `genericIndex` to
		newStacks = [
			if i == from then newFrom
				else if i == to then newTo
				else stack
			| (i, stack) <- zip [0..] stacks]

applyMoves :: Bool -> Stacks -> [Move] -> Stacks
applyMoves doReverse = foldl (applyMove doReverse)

heads :: Stacks -> String
heads = map head

tests :: IO ()
tests = do
	check $ (heads $ applyMoves True stacks moves) == "CMZ"
	check $ (heads $ applyMoves False stacks moves) == "MCD"
	where
		(stacks, moves) = parseInput "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	(stacks, moves) <- getInput
	putStrLn $ heads $ applyMoves True stacks moves
	putStrLn $ heads $ applyMoves False stacks moves
