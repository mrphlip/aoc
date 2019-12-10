import Data.List
import Data.Function
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Exception
import Utils

type Point = (Integer, Integer)
data Direction = Direction Integer Integer deriving Show

readInput :: IO [Point]
readInput = do
	dat <- readFile "10.txt"
	return $ parseInput dat

parseInput :: String -> [Point]
parseInput str = [ (x,y) | (y, row) <- enumerate $ lines str, (x, cell) <- enumerate row, cell == '#' ]

getDirection :: Point -> Point -> Direction
getDirection (ax, ay) (bx, by) = Direction (bx - ax) (by - ay)

fromDirection :: Point -> Direction -> Point
fromDirection (x, y) (Direction dx dy) = (x + dx, y + dy)

instance Eq Direction where
	Direction ax ay == Direction bx by = (ax * by) == (bx * ay)
instance Ord Direction where
	-- sort into clockwise order - such that directly up is the lowest, and just
	-- left of up is the highest
	compare (Direction ax ay) (Direction bx by)
		| aseg /= bseg = compare aseg bseg
		| otherwise = compare (bx * ay) (ax * by)
		where
			segment x y
				| x == 0 && y < 0 = 1
				| x > 0 = 2
				| x == 0 && y > 0 = 3
				| x < 0 = 4
			aseg = segment ax ay
			bseg = segment bx by

getVisible :: Point -> [Point] -> Integer
getVisible target points = toInteger $ S.size $ S.fromList $ map (getDirection target) $ filter (/=target) $ points

findBest :: [Point] -> (Point, Integer)
findBest points = maximumBy (compare `on` snd) $ map (\x->(x, getVisible x points)) $ points

getDestroyOrder :: Point -> [Point] -> [Point]
getDestroyOrder target@(tx, ty) points = sortedpoints
	where
		groupedpoints :: M.Map Direction [Point]
		groupedpoints = M.fromListWith (++) $ [ (getDirection target p, [p]) | p <- points, p /= target ]
		sortedgroups :: M.Map Direction [Point]
		sortedgroups = fmap (sortBy sortfunc) groupedpoints
		sortfunc (ax, ay) (bx, by)
			|	ax < tx = compare bx ax
			| ax > tx = compare ax bx
			| ay < ty = compare by ay
			| ay > ty = compare ay by
		ratedpoints :: [(Point, (Integer, Direction))]
		ratedpoints = [ (p, (n, d)) | (d, ps) <- M.toList sortedgroups, (n, p) <- enumerate ps ]
		sortedpoints = map fst $ sortBy (compare `on` snd) $ ratedpoints

tests :: IO ()
tests = do
	check $ findBest (parseInput ".#..#\n.....\n#####\n....#\n...##") == ((3, 4), 8)
	check $ findBest (parseInput "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####") == ((5, 8), 33)
	check $ findBest (parseInput "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.") == ((1, 2), 35)
	check $ findBest (parseInput ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..") == ((6, 3), 41)
	let bigone = parseInput ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"
	check $ findBest bigone == ((11, 13), 210)

	let points = [(10,0),(-10,0),(0,10),(0,-10)] ++ [(x*sx,y*sy) | (x,y) <- [(8,1),(1,8),(5,5)], sx <- [-1,1], sy <- [-1,1]]
	let dirs = map (getDirection (0,0)) points
	let sorteddirs = sort dirs
	let sorted = map (fromDirection (0,0)) $ sorteddirs
	check $ sorted == [(0,-10),(1,-8),(5,-5),(8,-1),(10,0),(8,1),(5,5),(1,8),(0,10),(-1,8),(-5,5),(-8,1),(-10,0),(-8,-1),(-5,-5),(-1,-8)]

	check $ getDestroyOrder (8,3) (parseInput ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....#...###..\n..#.#.....#....##") == [
			(8,1),(9,0),(9,1),(10,0),(9,2),(11,1),(12,1),(11,2),(15,1),
			(12,2),(13,2),(14,2),(15,2),(12,3),(16,4),(15,4),(10,4),(4,4),
			(2,4),(2,3),(0,2),(1,2),(0,1),(1,1),(5,2),(1,0),(5,1),
			(6,1),(6,0),(7,0),(8,0),(10,1),(14,0),(16,1),(13,3),(14,3)
		]

	let order = getDestroyOrder (11,13) bigone
	check $ order !! 0 == (11,12)
	check $ order !! 1 == (12,1)
	check $ order !! 2 == (12,2)
	check $ order !! 9 == (12,8)
	check $ order !! 19 == (16,0)
	check $ order !! 49 == (16,9)
	check $ order !! 99 == (10,16)
	check $ order !! 198 == (9,6)
	check $ order !! 199 == (8,2)
	check $ order !! 200 == (10,9)
	check $ order !! 298 == (11,1)
	check $ length order == 299

	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	points <- readInput
	let (target, best) = findBest points
	print $ (target, best)
	print $ getDestroyOrder target points !! 199
