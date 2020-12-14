{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Bits
import Data.Char
import qualified Text.ParserCombinators.ReadP as P
import Control.Exception
import Utils

type Mask = (Integer, Integer)  -- (andmask, ormask)
data Operation = SetMask Mask | SetMem Integer Integer
type Memory = Array Integer Integer
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

applyMask :: Mask -> Integer -> Integer
applyMask (andmask, ormask) val = (val .&. andmask) .|. ormask

initState :: State
initState = (error "mask not set", listArray (0, 0) [0])

applyOperation :: State -> Operation -> State
applyOperation (_, mem) (SetMask mask) = (mask, mem)
applyOperation (mask, mem) (SetMem loc val) = (mask, setExpand loc (applyMask mask val) 0 mem)
applyOps :: [Operation] -> Memory
applyOps = snd . foldl applyOperation initState

getTotal :: Memory -> Integer
getTotal = sum . elems

tests :: IO ()
tests = do
	check $ (getTotal $ applyOps prog) == 165
	where
		prog = map readOperation ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", "mem[8] = 11", "mem[7] = 101", "mem[8] = 0"]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	prog <- getInput
	print $ getTotal $ applyOps prog
