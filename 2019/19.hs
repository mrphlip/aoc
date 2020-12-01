{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Ix
import Data.List
import Control.Exception
import Control.Monad
import Intcode
import Utils
import Direction

type Point = (Integer, Integer)
type BeamMap = Array Point Bool
type BoundsMap = Array Integer (Integer,Integer)

mapSize = 2000

getInput :: IO (IntcodeMem Integer)
getInput = do
	dat <- readFile "19.txt"
	return $ readProg dat

runProg :: IntcodeMem Integer -> Point -> Bool
runProg code (x, y) = (/=0) $ head $ icrunOutp $ icinitInp code [x,y]

beamMap :: IntcodeMem Integer -> BeamMap
beamMap code = listArray bounds [ runProg code p | p <- range bounds ]
	where bounds = ((0,0),(mapSize,mapSize))

partA :: BeamMap -> Integer
partA beammap = genericLength $ [ () | x <- [0..49], y <- [0..49], beammap ! (x,y) ]

showMap :: BeamMap -> String
showMap beammap = unlines rows
	where
		rows = [ row y | y <- [0..49] ]
		row y = [ cell x y | x <- [0..49] ]
		cell x y = if beammap ! (x,y) then '#' else '.'

findBounds :: BeamMap -> BoundsMap
findBounds beammap = listArray bounds [ (leftBound y, rightBound y) | y <- range bounds ]
	where
		bounds = (0,mapSize)
		-- As it happens, for my input the right bound is just less than y, and the left bound barely less than that
		leftBound y = let b = rightBound y in 1 + head [ x | x <- [b,b-1..0], not $ beammap ! (x,y) ]
		rightBound y = head [ x | x <- [y,y-1..0], beammap ! (x,y) ]

findSquare :: BoundsMap -> Point
findSquare bounds = (firstx, firsty)
	where
		isok y = snd (bounds ! y) - fst (bounds ! (y + 99)) >= 99
		firsty = binarysearch 1000 1500
		binarysearch miny maxy -- miny is the highest known not-good, maxy is the lowest known good
			| miny >= maxy - 1 = maxy
			| otherwise = let midy = (miny + maxy) `div` 2 in if isok midy then binarysearch miny midy else binarysearch midy maxy
		firstx = fst (bounds ! (firsty + 99))

main :: IO ()
main = do
	code <- getInput
	let beammap = beamMap code
	putStrLn $ showMap beammap
	print $ partA beammap

	let bounds = findBounds beammap
	print $ findSquare bounds
