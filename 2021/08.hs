{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Bits
import Data.List
import qualified Data.Map.Strict as M
import Control.Exception
import Control.Monad
import Utils

type Record = ([Int], [Int])

getInput :: IO [Record]
getInput = do
	dat <- readFile "08.txt"
	return $ map parseInputLine $ lines dat
parseInputLine :: String -> Record
parseInputLine line = (digits, output)
	where
		[digittxt, outputtxt] = split '|' line
		digits = map readBits $ words digittxt
		output = map readBits $ words outputtxt
readBits :: String -> Int
readBits bits = foldl (.|.) 0 $ map (bit.fst) $ filter ((`elem` bits).snd) $ zip [0..] ['a'..'g']

countPartA :: [Record] -> Integer
countPartA records = genericLength $ filter ((`elem`[2,3,4,7]).popCount) $ join $ map snd records

single :: [a] -> a
single [x] = x
single [] = error "empty list"
single _ = error "too many values in list"

(.-.) :: (Bits a) => a -> a -> a
a .-. b = a .&. complement b
subsetOf :: (Bits a) => a -> a -> Bool
a `subsetOf` b = a .-. b == zeroBits

solveMapping :: [Int] -> M.Map Int Char
solveMapping vals = M.fromList [(digit i, i) | i <- ['0'..'9']]
	where
		doubles = filter ((==2).popCount) vals
		trebles = filter ((==3).popCount) vals
		quads = filter ((==4).popCount) vals
		quints = filter ((==5).popCount) vals
		hexes = filter ((==6).popCount) vals
		septs = filter ((==7).popCount) vals
		digit '0' = single $ filter (/= digit '6') $ filter (/= digit '9') hexes
		digit '1' = single $ doubles
		digit '2' = single $ filter (/= digit '3') $ filter (/= digit '5') quints
		digit '3' = single $ filter (digit '1' `subsetOf`) quints
		digit '4' = single $ quads
		digit '5' = single $ filter ((digit '4' .-. digit '1') `subsetOf`) quints
		digit '6' = single $ filter (not.(digit '1' `subsetOf`)) hexes
		digit '7' = single $ trebles
		digit '8' = single $ septs
		digit '9' = single $ filter (digit '4' `subsetOf`) hexes

solve :: Record -> Integer
solve (vals, output) = read $ map (mapping M.!) output
	where mapping = solveMapping vals

tests :: IO ()
tests = do
	check $ countPartA complex == 26
	check $ solve simple == 5353
	check $ (sum $ map solve complex) == 61229
	where
		simple = parseInputLine "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
		complex = map parseInputLine $ lines "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	records <- getInput
	print $ countPartA records
	print $ sum $ map solve records
