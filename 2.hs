import Data.Array
import Data.List
import Control.Exception
import Control.Monad
import Intcode
import Utils

type Val = Integer

getInput :: IO (IntcodeMem Val)
getInput = do
	dat <- readFile "2.txt"
	return $ readProg dat

tryVals :: IntcodeMem Val -> Val -> Val -> Val
tryVals prog a b = let (_, result, _, _, _) = icrun (0, prog // [(1, a), (2, b)], [], id, 0) in result ! 0

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
