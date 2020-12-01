{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.List
import Data.Char
import Intcode
import Utils

getInput :: IO (IntcodeMem Integer)
getInput = do
	dat <- readFile "21.txt"
	return $ readProg dat

runProg :: IntcodeMem Integer -> String -> String
runProg prog str = shown
	where
		inputs = map (toInteger.ord) str
		outputs = icrunOutp $ icinitInp prog inputs
		shown = do
			outp <- outputs
			if outp < 256
				then [chr $ fromInteger outp]
				else "[" ++ show outp ++ "]"

partA = unlines [
	-- There is a hole in front of us
	"NOT A J",
	"NOT B T",
	"OR T J",
	"NOT C T",
	"OR T J",
	-- and somewhere to land
	"AND D J",
	"WALK", ""]
partB = unlines [
	-- There is a hole in front of us
	"NOT A J",
	"NOT B T",
	"OR T J",
	"NOT C T",
	"OR T J",
	-- and somewhere to land
	"AND D J",
	-- reset temp register (if J is true this will set T to false; if J is false we don't care how T ends up)
	"NOT J T",
	-- and we can continue safely from there
	-- can we take two steps, or take a step and then jump?
	"OR I T",
	"OR F T",
	"AND E T",
	-- or jump again immediately?
	"OR H T",
	-- combine that with the first half
	"AND T J",
	"RUN", ""]
main :: IO ()
main = do
	prog <- getInput
	putStrLn $ runProg prog partA
	putStrLn $ runProg prog partB
