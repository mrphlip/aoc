{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.List
import Control.Exception
import Control.Monad
import Intcode
import Utils

type Val = Integer

getInput :: IO (IntcodeMem Val)
getInput = do
	dat <- readFile "5.txt"
	return $ readProg dat

main :: IO ()
main = do
	code <- getInput
	print $ icrunOutp $ icinitInp code [1]
	print $ icrunOutp $ icinitInp code [5]
