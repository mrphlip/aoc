{-# OPTIONS_GHC -Wno-tabs #-}
{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Tuple
import qualified Data.Map.Strict as M
import Control.Exception

vanEck_attempt1 :: [Integer] -> [Integer]
vanEck_attempt1 starts = starts ++ worker initmap finalval (genericLength starts - 1)
	where
		initmap = M.fromList $ map swap $ zip [0..] $ init starts
		finalval = last starts
		calcnextval !lastseen !lastval !len
			| lastval `M.member` lastseen = len - (lastseen M.! lastval)
			| otherwise = 0
		worker !lastseen !lastval !len = nextval:worker newmap nextval (len + 1)
			where
				!nextval = calcnextval lastseen lastval len
				!newmap = M.insert lastval len lastseen

vanEck_attempt2 :: [Integer] -> [Integer]
vanEck_attempt2 starts = starts ++ unfoldr worker (initmap, finalval, genericLength starts - 1)
	where
		initmap = M.fromList $ map swap $ zip [0..] $ init starts
		finalval = last starts
		calcnextval !lastseen !lastval !len
			| lastval `M.member` lastseen = len - (lastseen M.! lastval)
			| otherwise = 0
		worker (!lastseen, !lastval, !len) = Just (nextval, (newmap, nextval, len + 1))
			where
				!nextval = calcnextval lastseen lastval len
				!newmap = M.insert lastval len lastseen

vanEck_attempt3 :: [Integer] -> [Integer]
vanEck_attempt3 starts = results
	where
		startlen = length starts
		results = starts ++ (drop (startlen - 1) $ zipWith3 calcnext lastseenmaps results [0..])
		lastseenmaps = scanl (\m (ix,x) -> M.insert x ix m) M.empty $ zip [0..] results
		calcnext !lastseen !lastval !len
			| lastval `M.member` lastseen = len - (lastseen M.! lastval)
			| otherwise = 0

vanEck = vanEck_attempt1

tests :: IO ()
tests = do
	check $ take 10 (vanEck [0,3,6]) == [0,3,6,0,3,3,1,0,4,0]
	check $ vanEck [0,3,6] !! 2019 == 436
	check $ vanEck [1,3,2] !! 2019 == 1
	check $ vanEck [2,1,3] !! 2019 == 10
	check $ vanEck [1,2,3] !! 2019 == 27
	check $ vanEck [2,3,1] !! 2019 == 78
	check $ vanEck [3,2,1] !! 2019 == 438
	check $ vanEck [3,1,2] !! 2019 == 1836
	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	print $ vanEck [14,1,17,0,3,20] !! 2019
	print $ vanEck [14,1,17,0,3,20] !! 29999999
