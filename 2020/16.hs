{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Ix
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Control.Exception
import Control.Monad
import qualified Range as R
import qualified Text.ParserCombinators.ReadP as P
import Debug.Trace
import Utils

getInput :: IO ([(String, R.Ranges Integer)], [Integer], [[Integer]])
getInput = do
	dat <- readFile "16.txt"
	return $ readInput dat

readInput :: String -> ([(String, R.Ranges Integer)], [Integer], [[Integer]])
readInput s = (ranges, ticket, nearby)
	where
		[rangesstr, ticketstr, nearbystr] = splitOn "\n\n" s
		ranges = map parseRange $ lines rangesstr
		parseRange rangestr = runReadP readRangeLine rangestr
			where
				readRangeLine = do
					name <- P.munch1 (/=':')
					P.char ':'
					P.skipSpaces
					pairs <- P.sepBy1 readPair $ P.skipSpaces >> P.string "or" >> P.skipSpaces
					return (name, R.fromPairs pairs)
				readPair = do
					a <- readInt
					P.skipSpaces
					P.char '-'
					P.skipSpaces
					b <- readInt
					return (a, b)
				readInt = P.readS_to_P reads :: P.ReadP Integer
		[myticketstr] = tail $ lines ticketstr
		ticket = parseTicket myticketstr
		nearbystrs = tail $ lines nearbystr
		nearby = map parseTicket nearbystrs
		parseTicket = map read . splitOn ","

invalidVals :: [R.Ranges Integer] -> [[Integer]] -> [Integer]
invalidVals ranges tickets = [ val | ticket <- tickets, val <- ticket, not $ R.inRange val fullrange ]
	where fullrange = foldl1 R.union ranges

validTickets :: [R.Ranges Integer] -> [[Integer]] -> [[Integer]]
validTickets ranges tickets = filter (all $ flip R.inRange fullrange) tickets
	where fullrange = foldl1 R.union ranges

buildGrid :: [R.Ranges Integer] -> [[Integer]] -> Array (Integer, Integer) Bool
buildGrid ranges tickets = array bounds cells
	where
		transtickets = transpose $ validTickets ranges tickets
		size = genericLength ranges
		bounds = ((0, 0), (size - 1, size - 1))
		cells = do
			(x, range) <- enumerate ranges
			(y, vals) <- enumerate transtickets
			let valid = all (flip R.inRange range) vals
			return ((x, y), valid)

solveGrid :: Array (Integer, Integer) Bool -> [(Integer, Integer)]
solveGrid initgrid = solver initgrid (S.fromAscList [0..size]) (S.fromAscList [0..size])
	where
		(_, (size, _)) = bounds initgrid
		solver grid unsolvedrows unsolvedcols
			| S.null unsolvedrows || S.null unsolvedcols = []
			| otherwise = results ++ solver grid' unsolvedrows' unsolvedcols'
			where
				candidatesrow = do
					n <- [0..size]
					guard $ n `S.member` unsolvedrows
					let opts = filter (\m -> grid ! (n,m)) [0..size]
					guard $ length opts == 1
					return (n, head opts)
				candidatescol = do
					n <- [0..size]
					guard $ n `S.member` unsolvedcols
					let opts = filter (\m -> grid ! (m,n)) [0..size]
					guard $ length opts == 1
					return (head opts, n)
				candidates = candidatesrow ++ candidatescol
				results = if null candidates then error "No candidates" else nub candidates
				unsolvedrows' = foldl (flip S.delete) unsolvedrows $ map fst results
				unsolvedcols' = foldl (flip S.delete) unsolvedcols $ map snd results
				updatesrow = [ ((z,y),False) | (x,y) <- results, z <- [0..size], z /= x ]
				updatescol = [ ((x,z),False) | (x,y) <- results, z <- [0..size], z /= y ]
				grid' = grid // updatesrow // updatescol

decodeResult :: [String] -> [Integer] -> [(Integer, Integer)] -> [(String, Integer)]
decodeResult categories ticket pairs = do
	(row, col) <- pairs
	let cat = categories `genericIndex` row
	let val = ticket `genericIndex` col
	return (cat, val)

solveTicket :: [(String, R.Ranges Integer)] -> [Integer] -> [[Integer]] -> [(String, Integer)]
solveTicket ranges ticket nearby = decodeResult (map fst ranges) ticket $ solveGrid $ buildGrid (map snd ranges) nearby

calcPartB :: [(String, Integer)] -> Integer
calcPartB pairs = foldl1 (*) $ map snd $ filter ((=="departure ").take 10.fst) $ pairs

tests = do
	check $ (sum $ invalidVals (map snd testrangesA) testnearbyA) == 71
	check $ (sort $ solveTicket testrangesB testticketB testnearbyB) == [("class", 12), ("row", 11), ("seat", 13)]
	where
		(testrangesA, testticketA, testnearbyA) = readInput "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12"
		(testrangesB, testticketB, testnearbyB) = readInput "class: 0-1 or 4-19\nrow: 0-5 or 8-19\nseat: 0-13 or 16-19\n\nyour ticket:\n11,12,13\n\nnearby tickets:\n3,9,18\n15,1,5\n5,14,9"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	(ranges, ticket, nearby) <- getInput
	print $ sum $ invalidVals (map snd ranges) nearby
	print $ calcPartB $ solveTicket ranges ticket nearby
