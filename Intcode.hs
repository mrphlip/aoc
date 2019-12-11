module Intcode (IntcodeMem, Intcode, icinit, icinitInp, icstep, icrun, icrunMem, icrunOutp, readProg) where

import Data.Array
import Data.List
import Data.Maybe
import Data.Ix
import Control.Exception
import Control.Monad
import Debug.Trace
import Utils

type IntcodeMem a = Array a a
type Intcode a = (a, IntcodeMem a, [a], [a] -> [a], a) -- (IP, Memory, Input, Output, Base)

readProg :: (Ix a, Num a, Read a) => String -> IntcodeMem a
readProg s = listArrayLen $ map read $ split ',' $ takeWhile (/='\n') s

icinit :: (Ix a, Num a) => IntcodeMem a -> Intcode a
icinit prog = (0, prog, [], id, 0)

icinitInp :: (Ix a, Num a) => IntcodeMem a -> [a] -> Intcode a
icinitInp prog inp = (0, prog, inp, id, 0)

icstep :: (Ix a, Integral a, Show a) => Intcode a -> Maybe (Intcode a)
--icstep (ip, mem, inp, outp, base) = traceShow (ip, [mem ! x | x <- [ip..min (ip+3) (snd $ bounds mem)]], listToMaybe inp, listToMaybe (outp []), base) $ icstep_ (ip, mem, inp, outp, base)
icstep = icstep_

changeBounds :: (Ix i) => (i,i) -> a -> Array i a -> Array i a
changeBounds newrange def arr = cleared
	where
		oldrange = bounds arr
		expanded = ixmap newrange ixmapfunc arr
		ixmapfunc ix
			| inRange oldrange ix = ix
			| otherwise = fst oldrange
		cleared = expanded // [ (ix,def) | ix <- range newrange, not $ inRange oldrange ix ]

icstep_ :: (Ix a, Integral a, Show a) => Intcode a -> Maybe (Intcode a)
icstep_ (ip, mem, inp, outp, base)
	| opcode == 1 = Just (ip + 4, setop 3 (val1 + val2), inp, outp, base)  -- Add
	| opcode == 2 = Just (ip + 4, setop 3 (val1 * val2), inp, outp, base)  -- Multiply
	| opcode == 3 = Just (ip + 2, setop 1 (head inp), tail inp, outp, base)  -- Input
	| opcode == 4 = Just (ip + 2, mem, inp, outp . (val1:), base) -- Output
	| opcode == 5 = Just (if val1 /= 0 then val2 else ip + 3, mem, inp, outp, base) -- JNZ
	| opcode == 6 = Just (if val1 == 0 then val2 else ip + 3, mem, inp, outp, base) -- JZ
	| opcode == 7 = Just (ip + 4, setop 3 (if val1 < val2 then 1 else 0), inp, outp, base) -- less-than
	| opcode == 8 = Just (ip + 4, setop 3 (if val1 == val2 then 1 else 0), inp, outp, base) -- equal-to
	| opcode == 9 = Just (ip + 2, mem, inp, outp, base + val1) -- Adjust base
	| opcode == 99 = Nothing
	where
		instr = getval ip
		opcode = instr `mod` 100
		mode 1 = (instr `div` 100) `mod` 10
		mode 2 = (instr `div` 1000) `mod` 10
		mode 3 = (instr `div` 10000) `mod` 10
		getoper 0 x = getval x
		getoper 1 x = x
		getoper 2 x = getval (x + base)
		setoper 0 x v = setval x v
		setoper 1 x v = throw $ AssertionFailed "write to immediate"
		setoper 2 x v = setval (x + base) v
		readop n = getoper (mode n) (getval (ip + n))
		setop n v = setoper (mode n) (getval (ip + n)) v
		val1 = readop 1
		val2 = readop 2
		val3 = readop 3
		getval ix
			| inRange (bounds mem) ix = mem ! ix
			| ix > 0 = 0
		setval ix val
			| inRange (bounds mem) ix = mem // [(ix, val)]
			| ix > 0 = changeBounds (0,ix) 0 mem // [(ix, val)]

icrun :: (Ix a, Integral a, Show a) => Intcode a -> Intcode a
icrun = last . (unfoldr iterfunc)
	where iterfunc = (liftM (\x->(x,x))) . icstep

icrunMem :: (Ix a, Integral a, Show a) => Intcode a -> IntcodeMem a
icrunMem machine = mem
	where (_,mem,_,_,_) = icrun machine
icrunOutp :: (Ix a, Integral a, Show a) => Intcode a -> [a]
icrunOutp machine = outp []
	where (_,_,_,outp,_) = icrun machine

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

	-- tests from 9
	let quine = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
	checkProg quine []  15 [] quine
	checkProg [1102,34915192,34915192,7,4,7,99] []  6 [] [1219070632396864]
	checkProg [104,1125899906842624,99] []  2 [] [1125899906842624]

	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"
		checkProg code inp  expip expmem expoutp = do
			--print code
			let (ip, mem, inpleft, outp, base) = icrun (0, listArrayLen code, inp, id, 0)
			check $ ip == expip
			check $ null expmem || elems mem == expmem
			check $ null inpleft
			check $ (outp []) == expoutp
