{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Function
import Data.List
import Data.Maybe
import Control.Exception
import Utils

getInput :: IO (Integer, [Maybe Integer])
getInput = do
	dat <- readFile "13.txt"
	return $ parseInput dat

parseInput :: String -> (Integer, [Maybe Integer])
parseInput dat = (timestamp, ids)
	where
		[line1, line2] = lines dat
		timestamp = read line1
		ids = map readId $ split ',' line2
		readId "x" = Nothing
		readId n = Just $ read n

partA :: Integer -> [Maybe Integer] -> Integer
partA timestamp ids = result
	where
		realids = catMaybes ids
		getnextbus id = if timestamp `mod` id == 0 then timestamp else timestamp + id - (timestamp `mod` id)
		options = map (\id->(id, getnextbus id)) realids
		(nextbusid, nextbustime) = minimumBy (compare `on` snd) options
		result = nextbusid * (nextbustime - timestamp)

partB :: [Maybe Integer] -> Integer
partB ids = fst $ foldl1 chineseRemainder buses
	where
		buses = map (\(i, Just j) -> (-i, j)) $ filter (isJust . snd) $ enumerate ids

tests :: IO ()
tests = do
	check $ partA timestamp ids == 295
	check $ partB ids == 1068781
	where
		(timestamp, ids) = parseInput "939\n7,13,x,x,59,x,31,19"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	(timestamp, ids) <- getInput
	print $ partA timestamp ids
	print $ partB ids
