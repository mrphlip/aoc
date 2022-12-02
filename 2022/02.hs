{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Data.List.Split (splitOn)
import Control.Exception

data Throw = Rock | Paper | Scissors deriving (Eq, Ord, Enum, Show, Read)
data Result = Lose | Draw | Win deriving (Eq, Ord, Enum, Show, Read)

getInput :: IO [(Throw, Throw)]
getInput = do
	dat <- readFile "02.txt"
	return $ map parseInput $ lines dat

parseInput :: String -> (Throw, Throw)
parseInput [a, ' ', b] = (toThrow a 'A', toThrow b 'X')
	where toThrow x n = toEnum (fromEnum x - fromEnum n)

toResult :: Throw -> Throw -> Result
toResult you me = toEnum $ (fromEnum me - fromEnum you + 1) `mod` 3

fromResult :: Throw -> Result -> Throw
fromResult you res = toEnum $ (fromEnum you + fromEnum res - 1) `mod` 3

reinterpret :: Throw -> Result
reinterpret = toEnum . fromEnum

score :: Throw -> Result -> Integer
score t r = toInteger $ 1 + fromEnum t + 3 * fromEnum r

scoreA :: Throw -> Throw -> Integer
scoreA you me = score me res
	where res = toResult you me

scoreB :: Throw -> Throw -> Integer
scoreB you resT = score me res
	where res = reinterpret resT; me = fromResult you res

tests :: IO ()
tests = do
	check $ (sum $ map (uncurry scoreA) testData) == 15
	check $ (sum $ map (uncurry scoreB) testData) == 12
	where
		testData = map parseInput ["A Y", "B X", "C Z"]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	print $ sum $ map (uncurry scoreA) dat
	print $ sum $ map (uncurry scoreB) dat
