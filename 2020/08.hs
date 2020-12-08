{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import qualified Data.Set as S
import Control.Exception
import Utils

data Opcode = Acc Integer | Jmp Integer | Nop Integer deriving (Eq, Show, Read)
data State = State Integer Integer (S.Set Integer) | Halted Integer | Looped Integer deriving (Eq, Show, Read)
initState = State 0 0 S.empty

getInput :: IO [Opcode]
getInput = do
	dat <- readFile "08.txt"
	return $ map parseInputLine $ lines dat

parseInputLine :: String -> Opcode
parseInputLine ('a':'c':'c':' ':rest) = Acc $ readInt rest
parseInputLine ('j':'m':'p':' ':rest) = Jmp $ readInt rest
parseInputLine ('n':'o':'p':' ':rest) = Nop $ readInt rest

readInt :: String -> Integer
readInt ('+':rest) = read rest
readInt val = read val

nextState :: [Opcode] -> State -> Maybe State
nextState program (State ip acc seen) = Just realnextstate
	where
		State nextip nextacc nextseen = case program `genericIndex` ip of
			(Acc n) -> State (ip + 1) (acc + n) (S.insert ip seen)
			(Jmp n) -> State (ip + n) acc (S.insert ip seen)
			(Nop _) -> State (ip + 1) acc (S.insert ip seen)
		realnextstate
			| nextip `S.member` nextseen = Looped nextacc
			| nextip == genericLength program = Halted nextacc
			| otherwise = State nextip nextacc nextseen
nextState _ (Halted _) = Nothing
nextState _ (Looped _) = Nothing

iterState :: [Opcode] -> [State]
iterState program = unfoldr1 (nextState program) initState

lastState :: [Opcode] -> State
lastState = last . iterState

getAcc :: State -> Integer
getAcc (State _  acc _) = acc
getAcc (Halted acc) = acc
getAcc (Looped acc) = acc

listCandidates :: [Opcode] -> [[Opcode]]
listCandidates program = [ switchCode i | i <- [0..length program - 1], isSwitchable (program !! i) ]
	where
		isSwitchable (Acc _) = False
		isSwitchable _ = True
		switchOp (Jmp n) = Nop n
		switchOp (Nop n) = Jmp n
		switchCode n = take n program ++ [switchOp $ program !! n] ++ drop (n+1) program

getFixedLastState :: [Opcode] -> [State]
getFixedLastState = filter isHalted . map lastState . listCandidates
	where
		isHalted (Halted _) = True
		isHalted _ = False

tests = do
	check $ getAcc (lastState program) == 5
	check $ map getAcc (getFixedLastState program) == [8]
	where
		program = map parseInputLine $ lines "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	program <- getInput
	print $ getAcc $ lastState program
	print $ map getAcc $ getFixedLastState program
