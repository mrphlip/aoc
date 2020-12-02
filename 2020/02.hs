{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import qualified Text.ParserCombinators.ReadP as P
import Control.Exception

data Password = Password Integer Integer Char String deriving Eq

getInput :: IO [Password]
getInput = do
	dat <- readFile "02.txt"
	return $ map parseInputLine $ lines dat

parseInputLine :: String -> Password
parseInputLine line = fst $ head $ filter (null.snd) $ P.readP_to_S readLine line
	where
		readLine :: P.ReadP Password
		readLine = do
			low <- readInt
			P.char '-'
			high <- readInt
			P.skipSpaces
			char <- P.get
			P.char ':'
			P.skipSpaces
			password <- P.munch (const True)
			return $ Password low high char password
		readInt = P.readS_to_P reads :: P.ReadP Integer

validateA :: Password -> Bool
validateA (Password low high char password) = low <= count && count <= high
	where count = genericLength $ filter (==char) password

validateB :: Password -> Bool
validateB (Password low high char password) = isgood low /= isgood high
	where isgood n = (password `genericIndex` (n - 1)) == char

tests :: IO ()
tests = do
	check $ values == [Password 1 3 'a' "abcde", Password 1 3 'b' "cdefg", Password 2 9 'c' "ccccccccc"]
	check $ map validateA values == [True, False, True]
	check $ map validateB values == [True, False, False]
	where
		values = map parseInputLine ["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	passwords <- getInput
	print $ genericLength $ filter validateA passwords
	print $ genericLength $ filter validateB passwords
