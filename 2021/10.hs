{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Either
import Data.List
import Control.Exception
import Utils

type Rating = Either Integer Integer

getInput :: IO [String]
getInput = do
	dat <- readFile "10.txt"
	return $ lines dat

opening :: Char -> Bool
opening '(' = True
opening ')' = False
opening '[' = True
opening ']' = False
opening '{' = True
opening '}' = False
opening '<' = True
opening '>' = False
matching :: Char -> Char
matching '(' = ')'
matching '[' = ']'
matching '{' = '}'
matching '<' = '>'
pointsA :: Char -> Integer
pointsA ')' = 3
pointsA ']' = 3*19
pointsA '}' = 3*19*21
pointsA '>' = 3*19*21*21
pointsB :: Char -> Integer
pointsB ')' = 1
pointsB ']' = 2
pointsB '}' = 3
pointsB '>' = 4

rate :: String -> Rating
rate line = worker line []
	where
		worker :: String -> [Char] -> Rating
		worker (c:rest) stack | opening c = worker rest (matching c:stack)
		worker (c:rest) (expected:reststack) | c == expected = worker rest reststack
		worker (c:_) _ = Left $ pointsA c
		worker [] stack = Right $ fromBaseN 5 $ map pointsB stack

partA :: [String] -> Integer
partA dat = sum $ lefts $ map rate dat

partB :: [String] -> Integer
partB dat = if valcount `mod` 2 == 1 then vals !! (valcount `div` 2) else error "assertion failure: even number of results"
	where
		vals = sort $ rights $ map rate dat
		valcount = genericLength vals

tests :: IO ()
tests = do
	check $ (partA dat) == 26397
	check $ (partB dat) == 288957
	where
		dat = ["[({(<(())[]>[[{[]{<()<>>","[(()[<>])]({[<{<<[]>>(","{([(<{}[<>[]}>{[]{[(<()>","(((({<>}<{<{<>}{[]{[]{}","[[<[([]))<([[{}[[()]]]","[{[{({}]{}}([{[{{{}}([]","{<[[]]>}<{[{[{[]{()[[[]","[<(<(<(<{}))><([]([]()","<{([([[(<>()){}]>(<<{{","<{([{{}}[<[[[<>{}]]]>[]]"]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	print $ partA dat
	print $ partB dat
