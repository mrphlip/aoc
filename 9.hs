import Data.Array
import Data.List
import Control.Exception
import Control.Monad
import Intcode
import Utils

type Val = Integer

getInput :: IO (IntcodeMem Val)
getInput = do
	dat <- readFile "9.txt"
	return $ readProg dat

main :: IO ()
main = do
	code <- getInput
	let (_, _, _, output, _) = icrun (0, code, [1], id, 0)
	print $ output []
	let (_, _, _, output, _) = icrun (0, code, [2], id, 0)
	print $ output []
