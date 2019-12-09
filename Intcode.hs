module Intcode (IntcodeMem, Intcode, icstep, icrun, readProg) where

import Data.Array
import Data.List
import Data.Maybe
import Control.Exception
import Control.Monad
import Debug.Trace
import Utils

type IntcodeMem a = Array a a
type Intcode a = (a, IntcodeMem a, [a], [a] -> [a]) -- (IP, Memory, Input, Output)

readProg :: (Ix a, Num a, Read a) => String -> IntcodeMem a
readProg s = listArrayLen $ map read $ split ',' $ takeWhile (/='\n') s

icstep :: (Ix a, Integral a, Show a) => Intcode a -> Maybe (Intcode a)
--icstep (ip, mem, inp, outp) = traceShow (ip, [mem ! x | x <- [ip..min (ip+3) (snd $ bounds mem)]], listToMaybe inp, listToMaybe (outp [])) $ icstep_ (ip, mem, inp, outp)
icstep = icstep_

icstep_ :: (Ix a, Integral a, Show a) => Intcode a -> Maybe (Intcode a)
icstep_ (ip, mem, inp, outp)
	| opcode == 1 = assert (mode3 == 0) $ Just (ip + 4, mem // [(op3, val1 + val2)], inp, outp)  -- Add
	| opcode == 2 = assert (mode3 == 0) $ Just (ip + 4, mem // [(op3, val1 * val2)], inp, outp)  -- Multiply
	| opcode == 3 = assert (mode1 == 0) $ Just (ip + 2, mem // [(op1, head inp)], tail inp, outp)  -- Input
	| opcode == 4 = Just (ip + 2, mem, inp, outp . (val1:)) -- Output
	| opcode == 5 = Just (if val1 /= 0 then val2 else ip + 3, mem, inp, outp) -- JNZ
	| opcode == 6 = Just (if val1 == 0 then val2 else ip + 3, mem, inp, outp) -- JZ
	| opcode == 7 = assert (mode3 == 0) $ Just (ip + 4, mem // [(op3, if val1 < val2 then 1 else 0)], inp, outp) -- less-than
	| opcode == 8 = assert (mode3 == 0) $ Just (ip + 4, mem // [(op3, if val1 == val2 then 1 else 0)], inp, outp) -- equal-to
	| opcode == 99 = Nothing
	where
		instr = mem ! ip
		opcode = instr `mod` 100
		mode1 = (instr `div` 100) `mod` 10
		mode2 = (instr `div` 1000) `mod` 10
		mode3 = (instr `div` 10000) `mod` 10
		getval 0 x = mem ! x
		getval 1 x = x
		op1 = mem ! (ip + 1)
		val1 = getval mode1 op1
		op2 = mem ! (ip + 2)
		val2 = getval mode2 op2
		op3 = mem ! (ip + 3)
		val3 = getval mode3 op3

icrun :: (Ix a, Integral a, Show a) => Intcode a -> Intcode a
icrun = last . (unfoldr iterfunc)
	where iterfunc = (liftM (\x->(x,x))) . icstep

tests :: IO ()
tests = do
	-- tests from 2
	checkProg [1,9,10,3,2,3,11,0,99,30,40,50] []  8 [3500,9,10,70,2,3,11,0,99,30,40,50] []
	checkProg [1,0,0,0,99] []  4 [2,0,0,0,99] []
	checkProg [2,3,0,3,99] []  4 [2,3,0,6,99] []
	checkProg [2,4,4,5,99,0] []  4 [2,4,4,5,99,9801] []
	checkProg [1,1,1,4,99,5,6,0,99] []  8 [30,1,1,4,2,5,6,0,99] []

	-- tests from 5a
	checkProg [3,0,4,0,99] [123]  4 [] [123]
	checkProg [3,0,4,0,3,0,4,0,99] [123, 456]  8 [] [123, 456]
	checkProg [1002,4,3,4,33] []  4 [1002,4,3,4,99] []
	checkProg [1101,100,-1,4,0] []  4 [1101,100,-1,4,99] []

	-- tests from 5b
	checkProg [3,9,8,9,10,9,4,9,99,-1,8] [5]  8 [] [0]
	checkProg [3,9,8,9,10,9,4,9,99,-1,8] [8]  8 [] [1]
	checkProg [3,9,8,9,10,9,4,9,99,-1,8] [10]  8 [] [0]
	checkProg [3,9,7,9,10,9,4,9,99,-1,8] [5]  8 [] [1]
	checkProg [3,9,7,9,10,9,4,9,99,-1,8] [8]  8 [] [0]
	checkProg [3,9,7,9,10,9,4,9,99,-1,8] [10]  8 [] [0]
	checkProg [3,3,1108,-1,8,3,4,3,99] [5]  8 [] [0]
	checkProg [3,3,1108,-1,8,3,4,3,99] [8]  8 [] [1]
	checkProg [3,3,1108,-1,8,3,4,3,99] [10]  8 [] [0]
	checkProg [3,3,1107,-1,8,3,4,3,99] [5]  8 [] [1]
	checkProg [3,3,1107,-1,8,3,4,3,99] [8]  8 [] [0]
	checkProg [3,3,1107,-1,8,3,4,3,99] [10]  8 [] [0]

	checkProg [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [0]  11 [] [0]
	checkProg [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [123]  11 [] [1]
	checkProg [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [-1]  11 [] [1]
	checkProg [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [0]  11 [] [0]
	checkProg [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [123]  11 [] [1]
	checkProg [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [-1]  11 [] [1]

	let longtest_5b = [
			3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
			1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
			999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
		]
	checkProg longtest_5b [5]  46 [] [999]
	checkProg longtest_5b [8]  46 [] [1000]
	checkProg longtest_5b [10]  46 [] [1001]

	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"
		checkProg code inp  expip expmem expoutp = do
			--print code
			let (ip, mem, inpleft, outp) = icrun (0, listArrayLen code, inp, id)
			check $ ip == expip
			check $ null expmem || elems mem == expmem
			check $ null inpleft
			check $ (outp []) == expoutp
