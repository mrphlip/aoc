import Data.Array
import Data.List
import Intcode
import Utils
import Debug.Trace

getInput :: IO (IntcodeMem Integer)
getInput = do
	dat <- readFile "11.txt"
	return $ readProg dat

data Direction = LeftDir | RightDir | UpDir | DownDir deriving (Eq, Show, Read)
data Rotation = TurnLeft | TurnRight deriving (Eq, Show, Read)
type Point = (Integer, Integer)
data Colour = DefBlack | Black | White deriving (Eq, Show, Read)
type AntState = (Direction, Point, Array Point Colour)
initstate = (UpDir, (0,0), listArray ((0,0),(0,0)) [DefBlack])

step :: Direction -> Point -> Point
step LeftDir (x, y) = (x-1, y)
step RightDir (x, y) = (x+1, y)
step UpDir (x, y) = (x, y-1)
step DownDir (x, y) = (x, y+1)
rotate :: Rotation -> Direction -> Direction
rotate TurnLeft UpDir = LeftDir
rotate TurnLeft LeftDir = DownDir
rotate TurnLeft DownDir = RightDir
rotate TurnLeft RightDir = UpDir
rotate TurnRight UpDir = RightDir
rotate TurnRight RightDir = DownDir
rotate TurnRight DownDir = LeftDir
rotate TurnRight LeftDir = UpDir

readRotation 0 = TurnLeft
readRotation 1 = TurnRight
readColour 0 = Black
readColour 1 = White
writeColour DefBlack = 0
writeColour Black = 0
writeColour White = 1

--antstep state@(dir, pos, arr) rot col = traceShow (dir,pos,bounds arr) $ antstep_ state rot col
antstep = antstep_

antstep_ :: AntState -> Rotation -> Colour -> (AntState, Colour)
antstep_ (dir, pos, arr) rot col = ((dir', pos', arr'), getExpand pos' DefBlack arr)
	where
		arr' = setExpand pos col DefBlack arr
		dir' = rotate rot dir
		pos' = step dir' pos

makeinputs :: [Integer] -> ([Integer], AntState)
makeinputs outp = (0 : map fst results, snd $ last results)
	where
		iterfunc :: AntState -> [Integer] -> [(Integer, AntState)]
		iterfunc _ [] = []
		iterfunc state (col:rot:rest) = (writeColour col', state') : iterfunc state' rest
			where (state', col') = antstep state (readRotation rot) (readColour col)
		results :: [(Integer, AntState)]
		results = iterfunc initstate outp

runprog :: IntcodeMem Integer -> AntState
runprog code = finalstate
	where
		outputs = icrunOutp $ icinitInp code inputs
		(inputs, finalstate) = makeinputs outputs

showstate :: AntState -> String
showstate (_,_,arr) = unlines rows
	where
		((ax,ay),(bx,by)) = bounds arr
		rows = [ row y | y <- [ay..by] ]
		row y = [ cell x y | x <- [ax..bx] ]
		cell x y = showColour $ arr ! (x,y)
		showColour DefBlack = ' '
		showColour Black = '.'
		showColour White = '#'

countstate :: AntState -> Integer
countstate (_,_,arr) = genericLength $ filter (/=DefBlack) $ elems arr

main :: IO ()
main = do
	code <- getInput
	let finalstate = runprog code
	print $ bounds $ (\(_,_,x)->x) finalstate
	putStrLn $ showstate finalstate
	print $ countstate finalstate
