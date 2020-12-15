{-# OPTIONS_GHC -Wno-tabs #-}
{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Tuple
import qualified Data.Map.Strict as M
import Control.Exception
import Utils

{-
vanEck :: [Integer] -> [Integer]
vanEck starts = starts ++ unfoldr worker (initmap, finalval, genericLength starts - 1)
	where
		initmap = M.fromList $ map swap $ enumerate $ init starts
		finalval = last starts
		calcnextval !lastseen !lastval !len
			| lastval `M.member` lastseen = len - (lastseen M.! lastval)
			| otherwise = 0
		worker (!lastseen, !lastval, !len) = Just (nextval, (newmap, nextval, len + 1))
			where
				!nextval = calcnextval lastseen lastval len
				!newmap = M.insert lastval len lastseen
-}

vanEck :: [Integer] -> [Integer]
vanEck starts = starts ++ worker initmap finalval (genericLength starts - 1)
	where
		initmap = M.fromList $ map swap $ enumerate $ init starts
		finalval = last starts
		calcnextval !lastseen !lastval !len
			| lastval `M.member` lastseen = len - (lastseen M.! lastval)
			| otherwise = 0
		worker !lastseen !lastval !len = nextval:worker newmap nextval (len + 1)
			where
				!nextval = calcnextval lastseen lastval len
				!newmap = M.insert lastval len lastseen

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
