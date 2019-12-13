{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.List
import Control.Exception
import Control.Monad
import Intcode
import Utils

data Tile = Unset | Empty | Wall | Block | Paddle | Ball deriving (Eq, Show, Read)

getInput :: IO (IntcodeMem Integer)
getInput = do
	dat <- readFile "13.txt"
	return $ readProg dat

readTile 0 = Empty
readTile 1 = Wall
readTile 2 = Block
readTile 3 = Paddle
readTile 4 = Ball

showTile Unset = '~'
showTile Empty = ' '
showTile Wall = '#'
showTile Block = 'O'
showTile Paddle = '-'
showTile Ball = '*'

drawScreen :: [Integer] -> Array (Integer,Integer) Tile
drawScreen outp = iterfunc outp (listArray ((0,0),(0,0)) [Unset])
	where
		iterfunc [] arr = arr
		iterfunc (x:y:t:rest) arr = iterfunc rest $ setExpand (x,y) (readTile t) Unset arr

showScreen :: Array (Integer,Integer) Tile -> String
showScreen arr = unlines rows
	where
		((minx, miny), (maxx, maxy)) = bounds arr
		rows = [ row y | y <- [miny..maxy] ]
		row y = [ cell x y | x <- [minx..maxx] ]
		cell x y = showTile $ arr ! (x,y)

main :: IO ()
main = do
	code <- getInput
	let screen = drawScreen $ icrunOutp $ icinitInp code []
	putStrLn $ showScreen screen
	print $ length $ filter (==Block) $ elems screen
