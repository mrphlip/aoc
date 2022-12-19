{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Char
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Exception
import Utils
import Direction

type Point = (Integer, Integer)
type Well = (S.Set Point, Integer, Integer, Integer) -- (grid, well height, next piece, Direction sequence position)
type WellSummary = ([Integer], Integer, Integer) -- (column depths, next piece, Direction sequence position)
type LoopData = ([Integer], Integer, Integer, Integer) -- (well heights, loop start, loop end, additional height per loop)

emptyWell = (S.empty, 0, 0, 0) :: Well
wellWidth = 7
pieceShapes = [
		[
			[True, True, True, True]
		],
		[
			[False, True, False],
			[ True, True,  True],
			[False, True, False]
		],
		[
			[ True,  True, True],
			[False, False, True],
			[False, False, True]
		],
		[
			[True],
			[True],
			[True],
			[True]
		],
		[
			[True, True],
			[True, True]
		]
	]
pieceSize = [(4, 1), (3, 3), (3, 3), (1, 4), (2, 2)]

getInput :: IO [Direction]
getInput = parseInput <$> readFile "17.txt"

parseInput :: String -> [Direction]
parseInput s = map parseChar $ filter (not.isSpace) s
	where
		parseChar '<' = LeftDir
		parseChar '>' = RightDir

nextPiece :: [Direction] -> Well -> Well
nextPiece dirs (grid, wellHeight, pieceix, dirix) = (nextGrid, nextWellHeight, (pieceix + 1) `mod` genericLength pieceShapes, nextDirix)
	where
		pieceShape = pieceShapes `genericIndex` pieceix
		(pieceWidth, pieceHeight) = pieceSize `genericIndex` pieceix
		(finalp, nextDirix) = dropPiece (2, wellHeight + 3) dirix
		pieceAt (x, y) = [(px, py) | (py, row) <- zip [y..] pieceShape, (px, cell) <- zip [x..] row, cell]
		overlapAt p = any (`S.member` grid) $ pieceAt p
		pieceFits p@(x,y) = x >= 0 && (x + pieceWidth) <= wellWidth && y >= 0 && not (overlapAt p)
		dropPiece p dirix = if pieceFits tryVertMove then dropPiece tryVertMove nextDirix else (afterHorizMove, nextDirix)
			where
				nextDir = dirs `genericIndex` dirix
				nextDirix = (dirix + 1) `mod` genericLength dirs
				tryHorizMove = step nextDir p
				afterHorizMove = if pieceFits tryHorizMove then tryHorizMove else p
				tryVertMove = step UpDir afterHorizMove
		nextGrid = grid `S.union` S.fromList (pieceAt finalp)
		nextWellHeight = maximum $ wellHeight : [y+1 | (x,y) <- pieceAt finalp]

wellSummary :: Well -> WellSummary
wellSummary (grid, wellHeight, pieceix, dirix) = (map columnDepth [0..wellWidth-1], pieceix, dirix)
	where columnDepth x = genericLength $ takeWhile (\y -> not $ (x,y) `S.member` grid) $ [wellHeight-1,wellHeight-2..0]

findWellCycle :: [Direction] -> LoopData
findWellCycle dirs = (genericTake loopEnd wellHeights, loopStart, loopEnd, heightEnd - heightStart)
	where
		-- wells !! n is the well after dropping n blocks; similarly wellSummaries, wellHeights
		-- histories !! n is a map of all states up to, but not including, dropping n blocks
		-- so if wellSummaries!!n is a member of histories!!n then we have a loop
		wells = iterate (nextPiece dirs) emptyWell
		wellSummaries = map wellSummary wells
		wellHeights = map (\(_,h,_,_) -> h) wells
		histories :: [M.Map WellSummary Integer]
		histories = scanl (\m (i,w) -> M.insert w i m) M.empty $ zip [0..] wellSummaries
		(leadin, (loopSummary, loopHistory):_) = break (uncurry M.member) $ zip wellSummaries histories
		loopStart = (loopHistory M.! loopSummary)
		loopEnd = genericLength leadin
		heightStart = wellHeights `genericIndex` loopStart
		heightEnd = wellHeights `genericIndex` loopEnd

calcCycle :: LoopData -> Integer -> Integer
calcCycle (wellHeights, loopStart, loopEnd, loopHeight) step
	| step < loopEnd = wellHeights `genericIndex` step
	| otherwise = wellHeights `genericIndex` (loopStart + offset) + loopHeight * loops
	where
		(loops, offset) = (step - loopStart) `divMod` (loopEnd - loopStart)

tests :: IO ()
tests = do
	check $ calcCycle loopData 2022 == 3068
	check $ calcCycle loopData 1000000000000 == 1514285714288
	where
		testData = parseInput ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
		loopData = findWellCycle testData
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	let loopData = findWellCycle dat
	print $ calcCycle loopData 2022
	print $ calcCycle loopData 1000000000000
