{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Bits
import Data.Char
import qualified Data.Map.Strict as M
import qualified Text.ParserCombinators.ReadP as P
import Control.Exception
import Utils

type Mask = (Integer, Integer)  -- (andmask, ormask)
data Operation = SetMask Mask | SetMem Integer Integer
type Memory = M.Map Integer Integer
type State = (Mask, Memory)

getInput :: IO [Operation]
getInput = do
	dat <- readFile "14.txt"
	return $ map readOperation $ lines dat

readOperation :: String -> Operation
readOperation line = fst $ head $ filter (null.snd) $ P.readP_to_S readLine line
	where
		readLine :: P.ReadP Operation
		readLine = readMask P.+++ readSetMem
		readMask = do
			P.string "mask"
			P.skipSpaces
			P.char '='
			P.skipSpaces
			mask <- P.munch1 (not . isSpace)
			P.skipSpaces
			return $ SetMask $ calcMask mask
		readSetMem = do
			P.string "mem"
			P.skipSpaces
			P.char '['
			P.skipSpaces
			loc <- readInt
			P.skipSpaces
			P.char ']'
			P.skipSpaces
			P.char '='
			P.skipSpaces
			val <- readInt
			P.skipSpaces
			return $ SetMem loc val
		readInt = P.readS_to_P reads :: P.ReadP Integer

calcMask :: String -> Mask
calcMask mask = (andmask, ormask)
	where
		andmask = fromBaseN 2 $ map (\c -> if c == 'X' then 1 else 0) mask
		ormask = fromBaseN 2 $ map (\c -> if c == '1' then 1 else 0) mask

applyMaskA :: Mask -> Integer -> Integer
applyMaskA (andmask, ormask) val = (val .&. andmask) .|. ormask

toBits :: Integer -> [Int]
toBits n = filter (testBit n) $ takeWhile ((<=n).bit) [0..]

applyMaskB :: Mask -> Integer -> [Integer]
applyMaskB (andmask, ormask) val = map (((val .|. ormask) .&. complement andmask) .|.) $ map sum $ sequence $ map (\i -> [0, bit i]) $ toBits andmask

initState :: State
initState = (error "mask not set", M.empty)

applyOperationA :: State -> Operation -> State
applyOperationA (_, mem) (SetMask mask) = (mask, mem)
applyOperationA (mask, mem) (SetMem loc val) = (mask, M.insert loc (applyMaskA mask val) mem)
applyOpsA :: [Operation] -> Memory
applyOpsA = snd . foldl applyOperationA initState

applyOperationB :: State -> Operation -> State
applyOperationB (_, mem) (SetMask mask) = (mask, mem)
applyOperationB (mask, mem) (SetMem loc val) = (mask, M.union newmem mem)
	where
		locs = applyMaskB mask loc
		newmem = M.fromList [(l, val) | l <- locs]
applyOpsB :: [Operation] -> Memory
applyOpsB = snd . foldl applyOperationB initState

getTotal :: Memory -> Integer
getTotal = sum . M.elems

tests :: IO ()
tests = do
	check $ (getTotal $ applyOpsA progA) == 165
	check $ (getTotal $ applyOpsB progB) == 208
	where
		progA = map readOperation ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", "mem[8] = 11", "mem[7] = 101", "mem[8] = 0"]
		progB = map readOperation ["mask = 000000000000000000000000000000X1001X", "mem[42] = 100", "mask = 00000000000000000000000000000000X0XX", "mem[26] = 1"]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	prog <- getInput
	print $ getTotal $ applyOpsA prog
	print $ getTotal $ applyOpsB prog
