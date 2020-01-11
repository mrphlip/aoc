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
	dat <- readFile "02.txt"
	return $ readProg dat

tryVals :: IntcodeMem Val -> Val -> Val -> Val
tryVals prog a b = (!0) $ icrunMem $ icinit (prog // [(1, a), (2, b)])

testVals :: IntcodeMem Val -> Val -> Val -> Val -> IO ()
testVals prog a b target = do
	let catchfunc = (\e -> return 0) :: (SomeException -> IO Val)
	res <- handle catchfunc $ evaluate $ tryVals prog a b
	if res == target
		then print (a,b)
		else return ()

testAll :: IntcodeMem Val -> Val -> IO ()
testAll prog target = do
	sequence_ [testVals prog x y target | x <- indices prog, y <- indices prog]

main :: IO ()
main = do
	code <- getInput
	print $ tryVals code 12 2
	testAll code 19690720
