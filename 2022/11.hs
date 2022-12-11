{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.List
import qualified Text.ParserCombinators.ReadP as P
import Control.Exception
import Utils

type MonkeyHolds = Array Integer [Integer]
type MonkeyCounts = Array Integer Integer
type MonkeyInfo = (Integer -> Integer, Integer, Integer, Integer)
type MonkeyInfos = Array Integer MonkeyInfo
type MonkeyData = (MonkeyHolds, MonkeyCounts, MonkeyInfos)

data Operand = OperOld | OperVal Integer deriving (Eq, Ord, Show, Read)

getInput :: IO MonkeyData
getInput = parseInput <$> readFile "11.txt"

parseInput :: String -> MonkeyData
parseInput s = let res = runReadP readMonkeys s in
		(listArrayLen $ map fst res, listArrayLen $ map (const 0) res, listArrayLen $ map snd res)
	where
		readMonkeys :: P.ReadP [([Integer], MonkeyInfo)]
		readMonkeys = P.sepBy1 readMonkey (P.char '\n')
		readMonkey = do
			P.string "Monkey "
			readInt
			P.string ":\n"

			P.string "  Starting items: "
			hold <- P.sepBy1 readInt (P.string ", ")
			P.string "\n"

			P.string "  Operation: new = "
			opa <- readOperand
			P.char ' '
			oper <- readOperator
			P.char ' '
			opb <- readOperand
			P.string "\n"
			let operation = buildOperation oper opa opb

			P.string "  Test: divisible by "
			test <- readInt
			P.string "\n"

			P.string "    If true: throw to monkey "
			true <- readInt
			P.string "\n"

			P.string "    If false: throw to monkey "
			false <- readInt
			P.string "\n"

			return (hold, (operation, test, true, false))

		readInt = P.readS_to_P reads :: P.ReadP Integer
		readOperand = (OperOld <$ P.string "old") P.+++ (OperVal <$> readInt)
		readOperator = ((+) <$ P.string "+") P.+++ ((*) <$ P.string "*")

buildOperation :: (Integer -> Integer -> Integer) -> Operand -> Operand -> (Integer -> Integer)
buildOperation f (OperVal x) (OperVal y) = const (x `f` y)
buildOperation f (OperVal x) OperOld = (x `f`)
buildOperation f OperOld (OperVal x) = (`f` x)
buildOperation f OperOld OperOld = \n -> n `f` n

runMonkey :: (Integer -> Integer) -> MonkeyData -> Integer -> MonkeyData
runMonkey f (holds, counts, infos) n = (newholds, newcounts, infos)
	where
		tothrow = holds ! n
		(op, test, trueTarget, falseTarget) = infos ! n
		operated = map (f.op) tothrow
		(throwTrue, throwFalse) = partition ((==0).(`mod` test)) operated
		newholds = holds // [(n, []), (trueTarget, (holds ! trueTarget) ++ throwTrue), (falseTarget, (holds ! falseTarget) ++ throwFalse)]
		newcounts = counts // [(n, (counts ! n) + genericLength tothrow)]

runMonkeys :: (Integer -> Integer) -> MonkeyData -> MonkeyData
runMonkeys f ms@(h, _, _) = foldl (runMonkey f) ms $ indices h

runMonkeysMulti :: (Integer -> Integer) -> Integer -> MonkeyData -> MonkeyData
runMonkeysMulti f n ms = iterate (runMonkeys f) ms `genericIndex` n

partA :: Integer -> Integer
partA = (`div` 3)
partB :: MonkeyData -> Integer -> Integer
partB (_, _, infos) = (`mod` modulus)
	where modulus = foldl1 lcm $ map (\(_,test,_,_)->test) $ elems infos

rateResult :: MonkeyData -> Integer
rateResult (_, counts, _) = a * b
	where (a:b:_) = reverse $ sort $ elems counts

tests :: IO ()
tests = do
	check $ (rateResult $ runMonkeysMulti partA 20 testData) == 10605
	check $ (rateResult $ runMonkeysMulti (partB testData) 10000 testData) == 2713310158
	where
		testData = parseInput $ "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1\n"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	print $ rateResult $ runMonkeysMulti partA 20 dat
	print $ rateResult $ runMonkeysMulti (partB dat) 10000 dat
