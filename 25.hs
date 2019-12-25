{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.List
import Data.Char
import Data.Bits
import Data.Maybe
import Control.Monad
import Intcode
import Utils
import System.IO

getInput :: IO (IntcodeMem Integer)
getInput = do
	dat <- readFile "25.txt"
	return $ readProg dat

runprog :: IntcodeMem Integer -> String -> String
runprog prog = runprogFromState $ icinit prog

runprogFromState :: Intcode Integer -> String -> String
runprogFromState state inp = map (chr.fromInteger) $ icrunOutp $ appendInput inp state

runprogUntilNoInput :: Intcode Integer -> String -> String
runprogUntilNoInput state inp = map (chr.fromInteger) $ snd $ runUntilNoInput $ appendInput inp state

appendInput :: String -> Intcode Integer -> Intcode Integer
appendInput newinp (ip,code,inp,base,flag) = (ip,code,inp++newinpint,base,flag)
	where newinpint = map (toInteger.ord) newinp

runUntilNoInput :: Intcode Integer -> (Intcode Integer, [Integer])
runUntilNoInput state@(_,_,[],_,_) = (state, [])
runUntilNoInput state = if isNothing nextstep then (state, []) else (finalstate, fulloutp)
	where
		nextstep = icstep state
		Just (nextstate, outp) = nextstep
		(finalstate, restoutp) = runUntilNoInput nextstate
		fulloutp = if isNothing outp then restoutp else fromJust outp : restoutp

gather = "south\ntake astrolabe\nwest\ntake hologram\nsouth\ntake space law space brochure\nwest\ntake wreath\nwest\ntake hypercube\neast\neast\nnorth\neast\nsouth\ntake cake\nwest\nnorth\ntake coin\nsouth\neast\neast\nsouth\neast\ntake food ration\nsouth\n"
items = [
	"hologram",
	"food ration",
	"space law space brochure",
	"cake",
	"astrolabe",
	"wreath",
	"coin",
	"hypercube"]
after = "south\n\n"

afterScript :: String -> Intcode Integer -> Intcode Integer
afterScript script state = fst $ runUntilNoInput $ appendInput script state

makeDropCommands :: Integer -> String
makeDropCommands bits = join [ "drop " ++ item ++ "\n" | (i,item) <- enumerate items, bits .&. bit i == 0 ]

attempt :: Intcode Integer -> Integer -> String
attempt state bits = res
	where
		script = makeDropCommands bits ++ after
		outp = runprogUntilNoInput state script
		grepline line = take 26 line == "A loud, robotic voice says"
		found = [ line | line <- lines outp, grepline line ]
		res = case found of
			(first:_) -> first
			_ -> outp

-- mode 0 - interactive from the beginning
-- mode 1 - brute force the solution
-- mode 2 - run from after solution
mode = 0
main :: IO ()
main = do
	code <- getInput
	let state = afterScript gather $ icinit code
	case mode of
		0 -> interact $ runprog code
		1 -> do
			sequence_ $ flip map [0..255] $ \n -> do
				print n
				putStrLn $ attempt state n
				hFlush stdout
		2 -> do
			-- brute-force found that 201 is the solution
			let state' = appendInput after $ appendInput (makeDropCommands 201) $ state
			interact $ runprogFromState state'
