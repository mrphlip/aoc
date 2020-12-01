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
import Debug.Trace

type Point = (Integer, Integer)
type Maze = (Array Point Bool, M.Map Point (Point, Bool), M.Map String [(Point, Bool)], Point, Point)

getInput :: IO Maze
getInput = do
	dat <- readFile "20.txt"
	return $ readMaze dat

readMaze :: String -> Maze
readMaze s = (maze, portals, labelmap, start, end)
	where
		rows = lines s
		height = genericLength rows
		width = genericLength $ head rows
		map = array ((0,0),(width-1,height-1)) [ ((x,y),c) | (y,row) <- enumerate rows, (x,c) <- enumerate row ]
		maze = fmap (=='.') map

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
			let label = [map ! (x,y), map ! (x+dx,y+dy)]
			-- find out which direction is the floor tile this is labelling
			-- either one step in the opposite direction of dx,dy
			-- or two steps in the same direction of dx,dy
			let (lx, ly) = if getExpand (x-dx,y-dy) False maze then (-dx,-dy)
				else if getExpand (x+2*dx,y+2*dy) False maze then (2*dx,2*dy)
					else error $ "isolated label at " ++ show (x,y)
			-- is this label on the outside ring?
			let outside = x <= 2 || y <= 2 || x >= width - 3 || y >= height - 3
			return (label, [((x+lx, y+ly), outside)])
		labelmap = M.fromListWith (++) labels

		-- AA and ZZ should be labelling one point each - our start and end
		[(start, _)] = labelmap M.! "AA"
		[(end, _)] = labelmap M.! "ZZ"
		portalmap = M.delete "AA" $ M.delete "ZZ" $ labelmap

		-- th other labels should be labelling exactly two points - make a portal each way
		portals = M.fromList $ do
			[(p1, o1), (p2, o2)] <- M.elems portalmap
			if o1 == o2 then error $ "Two portals on same side " ++ show p1 ++ "; " ++ show p2
				else [(p1, (p2, o2)), (p2, (p1, o1))]

findNearestPortal :: Maze -> Maybe Integer
findNearestPortal (maze, portals, _, start, end) = fmap snd $ findNearest mapbounds start neighbours' (==end)
	where
		(mapbounds, neighbours, _) = makeArgsSquare maze [True] []
		neighbours' p = [ (p', (), n) | (p', _, n) <- neighbours p] ++
			if p `M.member` portals
				then [ (fst $ portals M.! p, (), 1) ]
				else []

findNearestPortalNest_direct :: Maze -> Integer -> Maybe Integer
findNearestPortalNest_direct (maze, portals, _, start, end) maxdepth = fmap snd $ findNearest expbounds (start,0) neighbours' (==(end,0))
	where
		((mapboundsmin, mapboundsmax), neighbours, _) = makeArgsSquare maze [True] []
		expbounds = ((mapboundsmin,0), (mapboundsmax,maxdepth))
		neighbours' (p,depth) = [ ((p',depth), (), n) | (p', _, n) <- neighbours p] ++
			if p `M.member` portals && if snd $ portals M.! p then depth < maxdepth else depth > 0
				then let (p', depthdir) = portals M.! p in [ ((p', depth + if depthdir then 1 else (-1)), (), 1) ]
				else []

calcRoutes :: Maze -> M.Map ((Integer, Bool), (Integer, Bool)) (Maybe Integer)
calcRoutes (maze, _, labels, start, end) = M.fromList $ do
	(label1, locs1) <- M.assocs labels
	(p1, outside1) <- locs1
	let (distmap, _) = buildDistMapSquare maze p1 [True] []
	(label2, locs2) <- M.assocs labels
	(p2, outside2) <- locs2
	let key = ((labelix label1, outside1), (labelix label2, outside2))
	let val = case distmap ! p2 of
		Just (val, _, _) -> Just val
		Nothing -> Nothing
	return (key, val)
	where
		labelnames = ["AA", "ZZ"] ++ (filter (/="AA") $ filter (/="ZZ") $ M.keys labels)
		labelix = (M.fromList [(l,n) | (n,l) <- enumerate labelnames] M.!)
findNearestPortalNest_precalc :: Maze -> Integer -> Maybe Integer
findNearestPortalNest_precalc maze@(_,_,labels,_,_) maxdepth = fmap snd $ findNearest mapbounds (0,True,0) neighbours (==(1,True,0))
	where
		routes = calcRoutes maze
		numlabels = toInteger $ M.size labels
		mapbounds = ((0, False, 0), (numlabels-1, True, maxdepth))
		neighbours (l, o, depth) = do
			l' <- [0..numlabels-1]
			o' <- [False, True]
			guard $ l /= l' || o /= o'
			guard $ l' >= 2 || o' -- there is no (0,False) or (1,False)
			let maybedist = routes M.! ((l, o), (l', o'))
			guard $ isJust maybedist
			let Just dist = maybedist
			-- is this a portal we can take?
			if l' >= 2 && if o' then depth > 0 else depth < maxdepth
				then [((l', o', depth), (), dist), ((l', not o', depth + if o' then (-1) else 1), (), dist + 1)]
				else [((l', o', depth), (), dist)]

--findNearestPortalNest = findNearestPortalNest_direct
findNearestPortalNest = findNearestPortalNest_precalc

tests :: IO ()
tests = do
	let sample1 = readMaze "         A           \n         A           \n  #######.#########  \n  #######.........#  \n  #######.#######.#  \n  #######.#######.#  \n  #######.#######.#  \n  #####  B    ###.#  \nBC...##  C    ###.#  \n  ##.##       ###.#  \n  ##...DE  F  ###.#  \n  #####    G  ###.#  \n  #########.#####.#  \nDE..#######...###.#  \n  #.#########.###.#  \nFG..#########.....#  \n  ###########.#####  \n             Z       \n             Z       "
	let sample1_noportals = (\(m,_,_,s,e)->(m,M.empty,M.empty,s,e)) sample1
	test $ (fromJust $ findNearestPortal sample1_noportals) == 26
	test $ (fromJust $ findNearestPortal sample1) == 23

	let sample2 = readMaze "                   A               \n                   A               \n  #################.#############  \n  #.#...#...................#.#.#  \n  #.#.#.###.###.###.#########.#.#  \n  #.#.#.......#...#.....#.#.#...#  \n  #.#########.###.#####.#.#.###.#  \n  #.............#.#.....#.......#  \n  ###.###########.###.#####.#.#.#  \n  #.....#        A   C    #.#.#.#  \n  #######        S   P    #####.#  \n  #.#...#                 #......VT\n  #.#.#.#                 #.#####  \n  #...#.#               YN....#.#  \n  #.###.#                 #####.#  \nDI....#.#                 #.....#  \n  #####.#                 #.###.#  \nZZ......#               QG....#..AS\n  ###.###                 #######  \nJO..#.#.#                 #.....#  \n  #.#.#.#                 ###.#.#  \n  #...#..DI             BU....#..LF\n  #####.#                 #.#####  \nYN......#               VT..#....QG\n  #.###.#                 #.###.#  \n  #.#...#                 #.....#  \n  ###.###    J L     J    #.#.###  \n  #.....#    O F     P    #.#...#  \n  #.###.#####.#.#####.#####.###.#  \n  #...#.#.#...#.....#.....#.#...#  \n  #.#####.###.###.#.#.#########.#  \n  #...#.#.....#...#.#.#.#.....#.#  \n  #.###.#####.###.###.#.#.#######  \n  #.#.........#...#.............#  \n  #########.###.###.#############  \n           B   J   C               \n           U   P   P               "
	test $ (fromJust $ findNearestPortal sample2) == 58

	let sample3 = readMaze "             Z L X W       C                 \n             Z P Q B       K                 \n  ###########.#.#.#.#######.###############  \n  #...#.......#.#.......#.#.......#.#.#...#  \n  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  \n  #.#...#.#.#...#.#.#...#...#...#.#.......#  \n  #.###.#######.###.###.#.###.###.#.#######  \n  #...#.......#.#...#...#.............#...#  \n  #.#########.#######.#.#######.#######.###  \n  #...#.#    F       R I       Z    #.#.#.#  \n  #.###.#    D       E C       H    #.#.#.#  \n  #.#...#                           #...#.#  \n  #.###.#                           #.###.#  \n  #.#....OA                       WB..#.#..ZH\n  #.###.#                           #.#.#.#  \nCJ......#                           #.....#  \n  #######                           #######  \n  #.#....CK                         #......IC\n  #.###.#                           #.###.#  \n  #.....#                           #...#.#  \n  ###.###                           #.#.#.#  \nXF....#.#                         RF..#.#.#  \n  #####.#                           #######  \n  #......CJ                       NM..#...#  \n  ###.#.#                           #.###.#  \nRE....#.#                           #......RF\n  ###.###        X   X       L      #.#.#.#  \n  #.....#        F   Q       P      #.#.#.#  \n  ###.###########.###.#######.#########.###  \n  #.....#...#.....#.......#...#.....#.#...#  \n  #####.#.###.#######.#######.###.###.#.#.#  \n  #.......#.......#.#.#.#.#...#...#...#.#.#  \n  #####.###.#####.#.#.#.#.###.###.#.###.###  \n  #.......#.....#.#...#...............#...#  \n  #############.#.#.###.###################  \n               A O F   N                     \n               A A D   M                     "

	test $ (fromJust $ findNearestPortalNest sample1 5) == 26
	test $ isNothing $ findNearestPortalNest sample2 5
	test $ (fromJust $ findNearestPortalNest sample3 10) == 396

main :: IO ()
main = do
	maze <- getInput
	print $ fromJust $ findNearestPortal maze
	print $ fromJust $ findNearestPortalNest maze 50
