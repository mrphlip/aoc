{-# OPTIONS_GHC -Wno-tabs #-}
import Utils
import Control.Exception

-- init, offset
type Operation = (Integer, Integer)

-- this really feels like it should be a Monoid, with
-- (mempty, mappend, mconcat) = (noop, addOperations size, combineOperations size)
-- but having to pass that extra "size" value around makes it real awkward
noop = (0,1) :: Operation

addOperations :: Integer -> Operation -> Operation -> Operation
addOperations size (init1, ofs1) (init2, ofs2) = ((init2 + init1 * ofs2) `mod` size, (ofs1 * ofs2) `mod` size)

combineOperations :: Integer -> [Operation] -> Operation
combineOperations size = foldr (addOperations size) noop

-- Lifting the implementation of (^) from Prelude
multiplyOperations :: Integer -> Operation -> Integer -> Operation
multiplyOperations size op n
	| n < 0 = error "Negative exponent"
	| n == 0 = noop
	| otherwise = f op n
	where
		f op n
			| even n = f (addOperations size op op) (n `quot` 2)
			| n == 1 = op
			| otherwise = g (addOperations size op op) (n `quot` 2) op
		g op n x
			| even n = g (addOperations size op op) (n `quot` 2) x
			| n == 1 = addOperations size op x
			| otherwise = g (addOperations size op op) (n `quot` 2) (addOperations size op x)

getInput :: IO [Operation]
getInput = do
	dat <- readFile "22.txt"
	return $ parseInput dat

parseInput :: String -> [Operation]
parseInput = map parseLine . lines

parseLine :: String -> Operation
parseLine "deal into new stack" = (-1, -1)
parseLine ('d':'e':'a':'l':' ':'w':'i':'t':'h':' ':'i':'n':'c':'r':'e':'m':'e':'n':'t':' ':rest) = (0, read rest)
parseLine ('c':'u':'t':' ':rest) = (-read rest, 1)

cardPosition :: Integer -> Operation -> Integer -> Integer
cardPosition size (init, ofs) n = (init + ofs * n) `mod` size

cardAt :: Integer -> Operation -> Integer -> Integer
cardAt size (init, ofs) n = ((n - init) * modRecip ofs size) `mod` size

tests :: IO ()
tests = do
	testShuffle "deal with increment 7\ndeal into new stack\ndeal into new stack" 10 (0,7)
	testShuffle "cut 6\ndeal with increment 7\ndeal into new stack" 10 (1,3)
	testShuffle "deal with increment 7\ndeal with increment 9\ncut -2" 10 (2,3)
	testShuffle "deal into new stack\ncut -2\ndeal with increment 7\ncut 8\ncut -4\ndeal with increment 7\ncut 3\ndeal with increment 9\ndeal with increment 3\ncut -1" 10 (7,7)
	where
		testShuffle s size res = test $ (combineOperations size $ parseInput s) == res

main :: IO ()
main = do
	ops <- getInput
	let partA = combineOperations 10007 ops
	print $ cardPosition 10007 partA 2019
	let partB = combineOperations 119315717514047 ops
	let repPartB = multiplyOperations 119315717514047 partB 101741582076661
	print $ cardAt 119315717514047 repPartB 2020
