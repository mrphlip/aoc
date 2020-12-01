{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Data.Function
import Control.Exception
import Intcode
import Utils

readInput = do
	dat <- readFile "07.txt"
	return $ readProg dat

runProgsA :: IntcodeMem Integer -> [Integer] -> Integer -> Integer
runProgsA _ [] val = val
runProgsA prog (arg:rest) val = runProgsA prog rest val'
	where
		[val'] = icrunOutp $ icinitInp prog [arg, val]

runProgsB :: IntcodeMem Integer -> [Integer] -> Integer -> Integer
runProgsB prog args init = last $ outputs !! (num - 1)
	where
		num = length args
		machines = [ icinitInp prog ((arg:) $ (if n == 0 then (init:) else id) $ outputs !! ((n - 1) `mod` num)) | (n, arg) <- enumerate args ]
		outputs = [ icrunOutp machine | machine <- machines ]

maximiseRun worker prog range = maximumBy (compare `on` snd) $ map (\x -> (x, worker prog x 0)) $ permutations range
maximiseRunA prog = maximiseRun runProgsA prog [0..4]
maximiseRunB prog = maximiseRun runProgsB prog [5..9]

tests = do
	check $ maximiseRunA (readProg "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0") == ([4,3,2,1,0], 43210)
	check $ maximiseRunA (readProg "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0") == ([0,1,2,3,4], 54321)
	check $ maximiseRunA (readProg "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0") == ([1,0,4,3,2], 65210)

	check $ maximiseRunB (readProg "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5") == ([9,8,7,6,5], 139629729)
	check $ maximiseRunB (readProg "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10") == ([9,7,8,5,6], 18216)
	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	prog <- readInput
	print $ maximiseRunA prog
	print $ maximiseRunB prog
