{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import qualified Text.ParserCombinators.ReadP as P
import Control.Exception
import Control.Monad
import Utils
import Range

type Point = (Integer, Integer)
type Beacons = [Point]
type Sensors = [(Point, Integer)]

getInput :: IO (Sensors, Beacons)
getInput = parseInput <$> lines <$> readFile "15.txt"

parseInput :: [String] -> (Sensors, Beacons)
parseInput xs = (sensors, beacons)
	where
		readInt = P.readS_to_P reads :: P.ReadP Integer
		readPoint = do
			P.string "x="
			x <- readInt
			P.string ", y="
			y <- readInt
			return (x, y)
		readData = do
			P.string "Sensor at "
			s <- readPoint
			P.string ": closest beacon is at "
			b <- readPoint
			return (s, b)

		sensorData = map (runReadP readData) xs
		sensors = [(s, abs (bx - sx) + abs (by - sy)) | (s@(sx, sy), (bx, by)) <- sensorData]
		beacons = nub $ map snd sensorData

getCoverage :: Sensors -> Integer -> Ranges Integer
getCoverage sensors y = fromPairs $ do
	((sx, sy), dist) <- sensors
	let remainingDist = dist - abs (y - sy)
	guard $ remainingDist >= 0
	return (sx - remainingDist, sx + remainingDist)

partA :: Sensors -> Beacons -> Integer -> Integer
partA sensors beacons y = coverage - beaconCount
	where
		coverage = rangeSize $ getCoverage sensors y
		beaconCount = genericLength $ filter ((==y).snd) beacons

partB :: Sensors -> Integer -> Integer
partB sensors mapSize = x * 4000000 + y
	where
		fullRange = fromPairs [(0, mapSize)]
		(x, y) = head $ do
			y <- [0..mapSize]
			let coverage = getCoverage sensors y
			let uncovered = toPairs $ Range.subtract fullRange coverage
			guard $ not $ null uncovered
			let (x, _) = head uncovered
			return (x, y)

tests :: IO ()
tests = do
	check $ partA sensors beacons 10 == 26
	check $ partB sensors 20 == 56000011
	where
		(sensors, beacons) = parseInput ["Sensor at x=2, y=18: closest beacon is at x=-2, y=15","Sensor at x=9, y=16: closest beacon is at x=10, y=16","Sensor at x=13, y=2: closest beacon is at x=15, y=3","Sensor at x=12, y=14: closest beacon is at x=10, y=16","Sensor at x=10, y=20: closest beacon is at x=10, y=16","Sensor at x=14, y=17: closest beacon is at x=10, y=16","Sensor at x=8, y=7: closest beacon is at x=2, y=10","Sensor at x=2, y=0: closest beacon is at x=2, y=10","Sensor at x=0, y=11: closest beacon is at x=2, y=10","Sensor at x=20, y=14: closest beacon is at x=25, y=17","Sensor at x=17, y=20: closest beacon is at x=21, y=22","Sensor at x=16, y=7: closest beacon is at x=15, y=3","Sensor at x=14, y=3: closest beacon is at x=15, y=3","Sensor at x=20, y=1: closest beacon is at x=15, y=3"]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	(sensors, beacons) <- getInput
	print $ partA sensors beacons 2000000
	print $ partB sensors 4000000
