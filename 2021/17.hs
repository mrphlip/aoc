{-# OPTIONS_GHC -Wno-tabs #-}
import qualified Text.ParserCombinators.ReadP as P
import Control.Exception
import Utils

type Point = (Integer, Integer)
type Bounds = (Point, Point)

getInput :: IO Bounds
getInput = do
	dat <- readFile "17.txt"
	return $ parseInput dat
parseInput :: String -> Bounds
parseInput = runReadP readInput
	where
		readInt = P.readS_to_P reads :: P.ReadP Integer
		readInput = do
			P.skipSpaces
			P.string "target area: x="
			xmin <- readInt
			P.string ".."
			xmax <- readInt
			P.string ", y="
			ymin <- readInt
			P.string ".."
			ymax <- readInt
			P.skipSpaces
			return ((xmin,ymin),(xmax,ymax))

velBounds :: Bounds -> Bounds
velBounds ((xmin,ymin),(xmax,ymax)) = ((1,ymin),(xmax,-ymin-1))

maxHeight :: Bounds -> Integer
maxHeight bounds = let ((_,_),(_,ymax)) = velBounds bounds in ymax * (ymax+1) `div` 2

hitTarget :: Bounds -> Point -> Bool
hitTarget ((xmin,ymin),(xmax,ymax)) (vx,vy) = worker 0 0 vx vy
	where worker x y vx vy
		| xmin <= x && x <= xmax && ymin <= y && y <= ymax = True
		| x > xmax || y < ymin = False
		| otherwise = worker (x+vx) (y+vy) (max 0 (vx-1)) (vy-1)

allOptions :: Bounds -> [Point]
allOptions bounds = [(vx,vy) | vx<-[vxmin..vxmax], vy<-[vymin..vymax], hitTarget bounds (vx,vy)]
	where ((vxmin,vymin),(vxmax,vymax)) = velBounds bounds

tests :: IO ()
tests = do
	check $ (maxHeight bounds) == 45
	check $ (length $ allOptions bounds) == 112
	where
		bounds = parseInput "target area: x=20..30, y=-10..-5"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	bounds <- getInput
	print $ maxHeight bounds
	print $ length $ allOptions bounds
