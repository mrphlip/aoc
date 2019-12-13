{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.List
import Control.Exception
import Control.Monad
import Control.Concurrent
import System.IO
import Intcode
import Utils

data Tile = Empty | Wall | Block | Paddle | Ball deriving (Eq, Show, Read)

getInput :: IO (IntcodeMem Integer)
getInput = do
	dat <- readFile "13.txt"
	return $ readProg dat

readTile 0 = Empty
readTile 1 = Wall
readTile 2 = Block
readTile 3 = Paddle
readTile 4 = Ball

showTile Empty = ' '
showTile Wall = '#'
showTile Block = 'O'
showTile Paddle = '-'
showTile Ball = '*'

drawScreen :: [Integer] -> Array (Integer,Integer) Tile
drawScreen outp = iterfunc outp $ listArray ((0,0),(0,0)) [Empty]
	where
		iterfunc [] arr = arr
		iterfunc (-2:rest) arr = iterfunc rest arr
		iterfunc (-1:0:_:rest) arr = iterfunc rest arr
		iterfunc (x:y:t:rest) arr = iterfunc rest $ setExpand (x,y) (readTile t) Empty arr

showScreen :: Array (Integer,Integer) Tile -> String
showScreen arr = unlines rows
	where
		((minx, miny), (maxx, maxy)) = bounds arr
		rows = [ row y | y <- [miny..maxy] ]
		row y = [ cell x y | x <- [minx..maxx] ]
		cell x y = showTile $ arr ! (x,y)

finalScore :: [Integer] -> Integer
finalScore outp = iterfunc outp 0
	where
		iterfunc [] score = score
		iterfunc (-2:rest) score = iterfunc rest score
		iterfunc (-1:0:score':rest) _ = iterfunc rest score'
		iterfunc (_:_:_:rest) score = iterfunc rest score

patchLoc = 75
thunkLoc = 10000
relocSize = 6
patchCode code = setExpand (thunkLoc + relocSize + 5) 0 0 code // patches
	where
		patches = join [patchMain, patchThunk, patchRelocate]
		patchMain = [(patchLoc, 1106), (patchLoc + 1, 0), (patchLoc + 2, thunkLoc)]
		patchThunk = [(thunkLoc, 104), (thunkLoc + 1, -2), (thunkLoc + relocSize + 2, 1106), (thunkLoc + relocSize + 3, 0), (thunkLoc + relocSize + 4, patchLoc + relocSize)]
		patchRelocate = [(thunkLoc + 2 + ix, code ! (patchLoc + ix)) | ix <- [0..relocSize-1]]

genInputs outp = iterfunc outp 0 0
	where
		iterfunc [] _ _ = []
		iterfunc (-2:rest) ball paddle = genInput ball paddle : iterfunc rest ball paddle
		iterfunc (-1:0:_:rest) ball paddle = iterfunc rest ball paddle
		iterfunc (x:_:3:rest) ball paddle = iterfunc rest ball x
		iterfunc (x:_:4:rest) ball paddle = iterfunc rest x paddle
		iterfunc (_:_:_:rest) ball paddle = iterfunc rest ball paddle
		genInput ball paddle
			| ball < paddle = -1
			| ball == paddle = 0
			| ball > paddle = 1

playGame code = outp
	where
		inp = genInputs outp
		outp = icrunOutp $ icinitInp code inp

frameDelay = 10000 -- microseconds
animScreen outp = do
	putStr "\x1B[2J" -- erase display
	putStr "\x1B[H" -- cursor position home
	putStr "  Score: 0"
	sequence_ $ process outp
	let maxy = maximum $ gety outp
	putStr $ "\x1B[" ++ show (maxy+3) ++ ";1H"
	where
		process [] = []
		process (-2:rest) = doFrameDelay : process rest
		process (-1:0:score:rest) = printscore score : process rest
		process (x:y:t:rest) = printtile x y t : process rest
		printscore score = putStr $ "\x1B[1;1H  Score: " ++ show score ++ "     "
		printtile x y t = putStr $ "\x1B[" ++ show (y+2) ++ ";" ++ show (x+1) ++ "H" ++ [showTile $ readTile t]
		doFrameDelay = do
			putStr "\x1B[H"
			hFlush stdout
			threadDelay frameDelay
		gety [] = []
		gety (-2:rest) = gety rest
		gety (-1:0:_:rest) = gety rest
		gety (_:y:_:rest) = y : gety rest

main :: IO ()
main = do
	code <- patchCode <$> getInput
	let outp = icrunOutp $ icinitInp code []
	let screen = drawScreen outp
	--putStrLn $ showScreen screen
	--animScreen outp
	print $ length $ filter (==Block) $ elems $ screen

	let outp2 = playGame (code // [(0,2)])
	--animScreen outp2
	print $ finalScore outp2
