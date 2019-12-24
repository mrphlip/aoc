{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Data.Array
import qualified Data.Map.Strict as M
import Data.Char
import Data.Maybe
import Control.Monad
import Utils
import Direction
import Dijkstra

type Point = (Integer, Integer)
type Maze = (Array Point Bool, M.Map Point Point, Point, Point)
data DirPortal = Direction Direction | Portal deriving (Eq, Show, Read)

getInput :: IO Maze
getInput = do
	dat <- readFile "20.txt"
	return $ readMaze dat

readMaze :: String -> Maze
readMaze s = (maze, portals, start, end)
	where
		rows = lines s
		height = genericLength rows
		width = genericLength $ head rows
		map = array ((0,0),(width-1,height-1)) [ ((x,y),c) | (y,row) <- enumerate rows, (x,c) <- enumerate row ]
		maze = fmap (=='.') map

		labels :: [(String, [Point])]
		labels = do
			x <- [0..width-1]
			y <- [0..height-1]
			-- find letters
			guard $ isLetter $ map ! (x,y)
			-- that aren't the second letter in a label
			guard $ not $ isLetter $ getExpand (x-1,y) ' ' map
			guard $ not $ isLetter $ getExpand (x,y-1) ' ' map
			-- find the second part of the letter
			let (dx, dy) = if isLetter $ getExpand (x+1,y) ' ' map then (1,0)
				else if isLetter $ getExpand (x,y+1) ' ' map then (0,1)
					else error $ "isolated letter at " ++ show (x,y)
			-- find out which direction is the floor tile this is labelling
			-- either one step in the opposite direction of dx,dy
			-- or two steps in the same direction of dx,dy
			let (lx, ly) = if getExpand (x-dx,y-dy) False maze then (-dx,-dy)
				else if getExpand (x+2*dx,y+2*dy) False maze then (2*dx,2*dy)
					else error $ "isolated label at " ++ show (x,y)
			return ([map ! (x,y), map ! (x+dx,y+dy)], [(x+lx, y+ly)])
		labelmap :: M.Map String [Point]
		labelmap = M.fromListWith (++) labels

		-- AA and ZZ should be labelling one point each - our start and end
		[start] = labelmap M.! "AA"
		[end] = labelmap M.! "ZZ"
		portalmap = M.delete "AA" $ M.delete "ZZ" $ labelmap

		-- th other labels should be labelling exactly two points - make a portal each way
		portals = M.fromList $ do
			[p1, p2] <- M.elems portalmap
			[(p1, p2), (p2, p1)]

findNearestPortal :: Maze -> Maybe ([DirPortal], Integer)
findNearestPortal (maze, portals, start, end) = findNearest mapbounds start neighbours' (==end)
	where
		(mapbounds, neighbours, _) = makeArgsSquare maze [True] []
		neighbours' p = [ (p', Direction d, n) | (p', d, n) <- neighbours p] ++
			if p `M.member` portals
				then [ (portals M.! p, Portal, 1) ]
				else []

tests :: IO ()
tests = do
	let sample1 = readMaze "         A           \n         A           \n  #######.#########  \n  #######.........#  \n  #######.#######.#  \n  #######.#######.#  \n  #######.#######.#  \n  #####  B    ###.#  \nBC...##  C    ###.#  \n  ##.##       ###.#  \n  ##...DE  F  ###.#  \n  #####    G  ###.#  \n  #########.#####.#  \nDE..#######...###.#  \n  #.#########.###.#  \nFG..#########.....#  \n  ###########.#####  \n             Z       \n             Z       "
	let sample1_noportals = (\(m,_,s,e)->(m,M.empty,s,e)) sample1
	test $ (snd $ fromJust $ findNearestPortal sample1_noportals) == 26
	test $ (snd $ fromJust $ findNearestPortal sample1) == 23
	let sample2 = readMaze "                   A               \n                   A               \n  #################.#############  \n  #.#...#...................#.#.#  \n  #.#.#.###.###.###.#########.#.#  \n  #.#.#.......#...#.....#.#.#...#  \n  #.#########.###.#####.#.#.###.#  \n  #.............#.#.....#.......#  \n  ###.###########.###.#####.#.#.#  \n  #.....#        A   C    #.#.#.#  \n  #######        S   P    #####.#  \n  #.#...#                 #......VT\n  #.#.#.#                 #.#####  \n  #...#.#               YN....#.#  \n  #.###.#                 #####.#  \nDI....#.#                 #.....#  \n  #####.#                 #.###.#  \nZZ......#               QG....#..AS\n  ###.###                 #######  \nJO..#.#.#                 #.....#  \n  #.#.#.#                 ###.#.#  \n  #...#..DI             BU....#..LF\n  #####.#                 #.#####  \nYN......#               VT..#....QG\n  #.###.#                 #.###.#  \n  #.#...#                 #.....#  \n  ###.###    J L     J    #.#.###  \n  #.....#    O F     P    #.#...#  \n  #.###.#####.#.#####.#####.###.#  \n  #...#.#.#...#.....#.....#.#...#  \n  #.#####.###.###.#.#.#########.#  \n  #...#.#.....#...#.#.#.#.....#.#  \n  #.###.#####.###.###.#.#.#######  \n  #.#.........#...#.............#  \n  #########.###.###.#############  \n           B   J   C               \n           U   P   P               "
	test $ (snd $ fromJust $ findNearestPortal sample2) == 58

main :: IO ()
main = do
	maze <- getInput
	print $ snd $ fromJust $ findNearestPortal maze
