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

type MachineState = (Intcode Integer, [Integer], Bool)
type NetworkState = ([MachineState], [Integer])
type Message = (Integer, [Integer])

icstepMulti :: NetworkState -> (NetworkState, [Message], [[Integer]])
icstepMulti (machines, nat) = ((newmachines, newnat), messages, if allidle then [nat] else [])
	where
		-- are all the input streams empty?
		allidle = all (\(_,_,idle) -> idle) machines
		natmachines = if allidle then natmachine0 : tail machines else machines
		natmachine0 = let ((ip, mem, _, base, flag), outps, _) = head machines in ((ip, mem, nat, base, flag), outps, False)
		-- run the machines
		afterstep = do
			((ip, mem, inp, base, flag), outps, idle) <- natmachines
			let emptyinput = null inp
			-- if the input stream is empty, feed it -1s
			let definput = if emptyinput then [-1] else inp
			-- run it for a step
			let Just ((ip', mem', inp', base', flag'), outp) = icstep (ip, mem, if null inp then [-1] else inp, base, flag)
			-- build the new machine state
			let outps' = case outp of {Just x -> outps ++ [x]; Nothing -> outps}
			let idle' = idle || (null inp && null inp') -- mark as idle if we consumed a -1
			return ((ip', mem', if null inp then [] else inp', base', flag'), outps', idle')
		-- if any machine has made three outputs, process that
		outputs = [outps | (_, outps, _) <- afterstep]
		messages = [ (to,[x,y]) | outps <- outputs, length outps >= 3, let (to:x:y:_) = outps]
		outputs' = [ if length outps >= 3 then drop 3 outps else outps | outps <- outputs ]
		-- process the messages
		messagemap = M.fromListWith (++) messages
		newmachines = do
			(n, (((ip, mem, inp, base, flag), _, idle), outps)) <- enumerate $ zip afterstep outputs'
			let inp' = inp ++ M.findWithDefault [] n messagemap
			let idle' = idle && not (n `M.member` messagemap) -- unset idle on receiving message
			return ((ip, mem, inp', base, flag), outps, idle')
		newnat = M.findWithDefault nat 255 messagemap

icrunMulti :: IntcodeMem Integer -> Integer -> ([Message], [[Integer]])
icrunMulti prog n = iterfunc initnetwork
	where
		initmachines = [(icinitInp prog [i], [], False) | i <- [0..n-1]]
		initnetwork = (initmachines, [-1, -1])
		iterfunc network = (messages ++ restmessages, natmessages ++ restnatmessages)
			where
				(newnetwork, messages, natmessages) = icstepMulti network
				(restmessages, restnatmessages) = iterfunc newnetwork

partA :: ([Message], [[Integer]]) -> [Integer]
partA (messages, _) = snd $ head $ dropWhile ((/=255).fst) messages

partB :: ([Message], [[Integer]]) -> [Integer]
partB (_, natmessages) = fst $ head $ dropWhile (\([x1,y1],[x2,y2])->y1/=y2) $ zip natmessages (tail natmessages)

main = do
	prog <- getInput
	let res = icrunMulti prog 50
	print $ partA $ res
	print $ partB $ res
	--let (_, natmessages) = res
	--sequence_ $ map print $ take 100 natmessages
