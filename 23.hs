{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Utils
import Intcode

getInput :: IO (IntcodeMem Integer)
getInput = do
	dat <- readFile "23.txt"
	return $ readProg dat

type MachineState = (Intcode Integer, [Integer])
type Message = (Integer, [Integer])

icstepMulti :: [MachineState] -> ([MachineState], [Message])
icstepMulti machines = (newmachines, messages)
	where
		-- if the input stream is empty, feed it -1s
		runmachines = [(ip, mem, if null inp then [-1] else inp, base, flag) | ((ip, mem, inp, base, flag), _) <- machines]
		-- run each machine once
		afterstep = map (fromJust . icstep) runmachines
		-- gather the outputs
		outputs = [case outp of {Just x -> outps ++ [x]; Nothing -> outps} | ((_, outps), (_, outp)) <- zip machines afterstep]
		-- if any machine has made three outputs, process that
		messages = [ (to,[x,y]) | outps <- outputs, length outps >= 3, let (to:x:y:_) = outps]
		outputs' = [ if length outps >= 3 then drop 3 outps else outps | outps <- outputs ]
		messagemap = M.fromListWith (++) messages
		newmachines = [((ip, mem, inp ++ M.findWithDefault [] n messagemap, base, flag), outps) | (n, (((ip, mem, inp, base, flag), _), outps)) <- enumerate $ zip afterstep outputs' ]

icrunMulti :: IntcodeMem Integer -> Integer -> [Message]
icrunMulti prog n = iterfunc initmachines
	where
		initmachines = [(icinitInp prog [i], []) | i <- [0..n-1]]
		iterfunc machines = let (newmachines, messages) = icstepMulti machines in messages ++ iterfunc newmachines

partA :: [Message] -> [Integer]
partA messages = snd $ head $ dropWhile ((/=255).fst) messages

main = do
	prog <- getInput
	print $ partA $ icrunMulti prog 50
