{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Control.Exception

getInput :: IO [[String]]
getInput = do
	dat <- readFile "06.txt"
	return $ parseInput dat

parseInput :: String -> [[String]]
parseInput = map lines . splitOn "\n\n"

countUnique :: [String] -> Integer
countUnique = toInteger . S.size . S.fromList . concat

countAll :: [String] -> Integer
countAll = toInteger . S.size . foldl1 S.intersection . map S.fromList

tests :: IO ()
tests = do
	check $ map countUnique answers == [3, 3, 3, 1, 1]
	check $ map countAll answers == [3, 0, 1, 1, 1]
	where
		answers = parseInput "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	answers <- getInput
	print $ sum $ map countUnique answers
	print $ sum $ map countAll answers
