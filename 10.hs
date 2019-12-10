import Data.List
import Data.Function
import qualified Data.Set as S
import Control.Exception
import Utils

type Point = (Integer, Integer)
data Direction = Direction Integer Integer

readInput :: IO [Point]
readInput = do
	dat <- readFile "10.txt"
	return $ parseInput dat

parseInput :: String -> [Point]
parseInput str = [ (x,y) | (y, row) <- enumerate $ lines str, (x, cell) <- enumerate row, cell == '#' ]

getDirection :: Point -> Point -> Direction
getDirection (ax, ay) (bx, by) = Direction (bx - ax) (by - ay)

instance Eq Direction where
	Direction ax ay == Direction bx by = (ax * by) == (bx * ay)
instance Ord Direction where
	-- Ordering is pretty arbitrary, it just needs to be consistent with (==)
	compare (Direction ax ay) (Direction bx by)
		| signum ay /= signum by = compare ay by
		| ay == 0 && by == 0 = compare (signum ax) (signum bx)
		| otherwise = compare (ax * by) (bx * ay)

getVisible :: Point -> [Point] -> Integer
getVisible target points = toInteger $ S.size $ S.fromList $ map (getDirection target) $ filter (/=target) $ points

findBest :: [Point] -> (Point, Integer)
findBest points = maximumBy (compare `on` snd) $ map (\x->(x, getVisible x points)) $ points

tests :: IO ()
tests = do
	check $ findBest (parseInput ".#..#\n.....\n#####\n....#\n...##") == ((3, 4), 8)
	check $ findBest (parseInput "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####") == ((5, 8), 33)
	check $ findBest (parseInput "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.") == ((1, 2), 35)
	check $ findBest (parseInput ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..") == ((6, 3), 41)
	check $ findBest (parseInput ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##") == ((11, 13), 210)
	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	points <- readInput
	print $ findBest points
