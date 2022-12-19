{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Control.Exception

type Point = (Integer, Integer, Integer)
type PointCloud = S.Set Point

getInput :: IO PointCloud
getInput = S.fromList <$> map parsePoint <$> lines <$> readFile "18.txt"

parsePoint :: String -> Point
parsePoint s = (read x, read y, read z)
	where [x, y, z] = splitOn "," s

neighbours :: Point -> [Point]
neighbours (x, y, z) = [(x + dx, y + dy, z + dz) | (dx, dy, dz) <- [(-1,0,0), (1,0,0), (0,-1,0), (0,1,0), (0,0,-1), (0,0,1)]]

surfaceArea :: (Point -> Bool) -> PointCloud -> Integer
surfaceArea pred points = genericLength [() | p <- S.elems points, n <- neighbours p, not $ n `S.member` points, pred n]

floodFillExterior :: PointCloud -> PointCloud
floodFillExterior points = worker (Q.singleton (minx, miny, minz)) S.empty
	where
		minx = (minimum $ map (\(x,y,z) -> x) $ S.elems points) - 1
		maxx = (maximum $ map (\(x,y,z) -> x) $ S.elems points) + 1
		miny = (minimum $ map (\(x,y,z) -> y) $ S.elems points) - 1
		maxy = (maximum $ map (\(x,y,z) -> y) $ S.elems points) + 1
		minz = (minimum $ map (\(x,y,z) -> z) $ S.elems points) - 1
		maxz = (maximum $ map (\(x,y,z) -> z) $ S.elems points) + 1
		worker :: Q.Seq Point -> PointCloud -> PointCloud
		worker Q.Empty exterior = exterior
		worker (next@(x,y,z) Q.:<| todo) exterior
			| x < minx || x > maxx || y < miny || y > maxy || z < minz || z > maxz = worker todo exterior
			| next `S.member` exterior = worker todo exterior
			| next `S.member` points = worker todo exterior
			| otherwise = worker (todo Q.>< Q.fromList (neighbours next)) (S.insert next exterior)

tests :: IO ()
tests = do
	check $ surfaceArea (const True) testData == 64
	check $ surfaceArea (`S.member` exterior) testData == 58
	where
		testData = S.fromList $ map parsePoint ["2,2,2","1,2,2","3,2,2","2,1,2","2,3,2","2,2,1","2,2,3","2,2,4","2,2,6","1,2,5","3,2,5","2,1,5","2,3,5"]
		exterior = floodFillExterior testData
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	let exterior = floodFillExterior dat
	print $ surfaceArea (const True) dat
	print $ surfaceArea (`S.member` exterior) dat
