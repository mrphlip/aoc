{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Control.Exception

getInput :: IO ([Integer], [Integer])
getInput = do
	dat <- readFile "22.txt"
	return $ readInput dat

readInput :: String -> ([Integer], [Integer])
readInput dat = (readDeck p1, readDeck p2)
	where
		[p1, p2] = splitOn "\n\n" dat
		readDeck = map read . tail . lines

doGameA :: [Integer] -> [Integer] -> (Integer, [Integer])
doGameA d1 [] = (1, d1)
doGameA [] d2 = (2, d2)
doGameA (c1:r1) (c2:r2) = if c1 > c2 then doGameA (r1++[c1,c2]) r2 else doGameA r1 (r2++[c2,c1])

doGameB :: [Integer] -> [Integer] -> (Integer, [Integer])
doGameB d1 d2 = worker d1 d2 S.empty
	where
		worker d1 [] _ = (1, d1)
		worker [] d2 _ = (2, d2)
		worker d1@(c1:r1) d2@(c2:r2) seen
			| (d1, d2) `S.member` seen = (1, d1)
			| otherwise = if winner == 1 then worker (r1++[c1,c2]) r2 seen' else worker r1 (r2++[c2,c1]) seen'
			where
				seen' = S.insert (d1, d2) seen
				winner
					| c1 <= genericLength r1 && c2 <= genericLength r2 = fst $ doGameB (genericTake c1 r1) (genericTake c2 r2)
					| otherwise = if c1 > c2 then 1 else 2

score :: [Integer] -> Integer
score d = sum $ zipWith (*) [1..] $ reverse d

tests :: IO ()
tests = do
	check $ doGameA p1 p2 == (2,[3,2,10,6,8,5,9,4,7,1])
	check $ (score $ snd $ doGameA p1 p2) == 306
	check $ doGameB p1 p2 == (2,[7,5,6,2,4,1,10,8,9,3])
	check $ (score $ snd $ doGameB p1 p2) == 291
	where
		(p1, p2) = ([9, 2, 6, 3, 1], [5, 8, 4, 7, 10])
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	(p1, p2) <- getInput
	let (winnerA, deckA) = doGameA p1 p2
	putStrLn $ "Game A won by player " ++ show winnerA
	print $ score deckA
	let (winnerB, deckB) = doGameB p1 p2
	putStrLn $ "Game B won by player " ++ show winnerB
	print $ score deckB
