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
	let (_, _, _, output) = icrun (0, code, [1], id)
	print $ output []
	let (_, _, _, output) = icrun (0, code, [5], id)
	print $ output []
