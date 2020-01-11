{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Data.Function
import Control.Exception
import Utils

data Pixel = White | Black | Transp deriving (Eq, Show, Read)

overlay :: Pixel -> Pixel -> Pixel
overlay Transp b = b
overlay a _ = a

overlayLayer = zipWith overlay

mergeLayers = foldr1 overlayLayer

readData = do
	dat <- readFile "08.txt"
	return $ parsePixels dat

parsePixels dat = map conv $ takeWhile (/='\n') dat
	where
		conv '0' = White
		conv '1' = Black
		conv '2' = Transp

splitLayers :: Integer -> Integer -> [Pixel] -> [[Pixel]]
splitLayers w h = chunk (w*h)

showImage :: Integer -> Integer -> [Pixel] -> String
showImage w h pixels = unlines rows
	where
		showpix White = ' '
		showpix Black = '@'
		showpix Transp = '~'
		shownpixels = map showpix pixels
		rows = chunk w shownpixels

countVals :: (Eq a) => a -> [a] -> Integer
countVals a = foldr counter 0
	where
		counter x n
			| x == a = n + 1
			| otherwise = n

checksum layers = (countVals Black minlayer) * (countVals Transp minlayer)
	where minlayer = minimumBy (compare `on` countVals White) layers

tests = do
	let layers1 = splitLayers 3 2 $ parsePixels "001122101212010020"
	check $ checksum layers1 == 6

	let layers2 = splitLayers 2 2 $ parsePixels "0222112222120000"
	check $ mergeLayers layers2 == [White, Black, Black, White]
	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	let w = 25
	let h = 6
	dat <- readData
	let layers = splitLayers w h dat
	print $ checksum layers
	putStrLn $ showImage w h $ mergeLayers layers
