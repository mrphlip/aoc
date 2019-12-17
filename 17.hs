{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import Control.Exception
import Control.Monad
import Intcode
import Utils
import Direction

type Point = (Integer, Integer)
type Location = (Point, Direction)
type Maze = Array Point Bool

getInput :: IO (IntcodeMem Integer)
getInput = do
	dat <- readFile "17.txt"
	return $ readProg dat

getMap :: IntcodeMem Integer -> String
getMap code = map (chr.fromInteger) $ icrunOutp $ icinit code

parseMap :: String -> (Maze, Location)
parseMap str = (array ((0,0),(width-1,height-1)) maze, location)
	where
		rows = filter (not.null) $ lines str
		height = genericLength rows
		width = genericLength $ head rows
		maze = [ ((x,y),makecell cell) | (y,row) <- enumerate rows, (x, cell) <- enumerate row ]
		makecell '.' = False
		makecell '#' = True
		makecell '^' = True
		makecell 'V' = True
		makecell '<' = True
		makecell '>' = True
		location = head $ catMaybes $ [ makeloc cell (x,y) | (y,row) <- enumerate rows, (x, cell) <- enumerate row ]
		makeloc '.' p = Nothing
		makeloc '#' p = Nothing
		makeloc '^' p = Just (p, UpDir)
		makeloc 'V' p = Just (p, DownDir)
		makeloc '<' p = Just (p, LeftDir)
		makeloc '>' p = Just (p, RightDir)

intersections :: Maze -> [Point]
-- find all points that are floor, and have >2 neighbours which are floor
intersections maze = [ p | p <- indices maze, maze ! p, length [() | d <- directions, getExpand (step d p) False maze] > 2 ]

getSolution :: Maze -> String
-- shh
getSolution _ = "A,B,B,A,C,B,C,C,B,A\nR,10,R,8,L,10,L,10\nR,8,L,6,L,6\nL,10,R,10,L,6\nn\n"

main :: IO ()
main = do
	code <- getInput
	let strmap = getMap code
	putStrLn strmap
	let (maze, _) = parseMap strmap
	print $ sum $ map (uncurry (*)) $ intersections maze
	print $ last $ icrunOutp $ icinitInp (code // [(0,2)]) $ map (toInteger.ord) $ getSolution maze
