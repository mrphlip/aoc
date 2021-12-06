{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import Control.Exception
import Utils

type Point = (Integer, Integer)
type Line = (Point, Point)

getInput :: IO [Integer]
getInput = do
	dat <- readFile "06.txt"
	return $ map read $ split ',' dat

getCounts :: [Integer] -> [Integer]
getCounts vals = map (fromMaybe 0 . (countmap M.!?)) [0..8]
	where countmap = counter vals

doStep :: [Integer] -> [Integer]
doStep vals = withresets
	where
		(zeros:rest) = vals
		nextvals = rest ++ [0]
		withresets = [if i == 6 || i == 8 then n+zeros else n | (i,n) <- zip [0..] nextvals]

doSteps :: Integer -> [Integer] -> Integer
doSteps n vals = sum $ iterate doStep vals `genericIndex` n

tests :: IO ()
tests = do
	check $ (doSteps 80 $ getCounts vals) == 5934
	check $ (doSteps 256 $ getCounts vals) == 26984457539
	where
		vals = [3,4,3,1,2]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	vals <- getInput
	print $ doSteps 80 $ getCounts vals
	print $ doSteps 256 $ getCounts vals
