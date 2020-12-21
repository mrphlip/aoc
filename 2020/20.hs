{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.Function
import Data.List
import Data.List.Split (splitOn)
import Data.Tuple
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Exception
import Control.Monad
import Utils
import Direction

type Tile = [[Bool]]
type Orientation = (Direction, Bool)
{-
Orientation is used for two purposes: to represent the rotation of a tile, and to
enumerate the sides of a tile...

For rotations, (o, f) indicates that the tile either is, or is not flipped, about
the X axis (according to f) and then rotated according to o (UpDir = no rotation)

For an edge, (d, r) means you're looking at the edge in the d direction, and reading
it left-to-right unless r is set, in which case it's reversed and read right-to-left.
-}
type Point = (Integer, Integer)
allOrients :: [Orientation]
allOrients = [(o,f) | o<-directions, f<-[False, True]]

getInput :: IO [(Integer, Tile)]
getInput = do
	dat <- readFile "20.txt"
	return $ readInput dat

readInput :: String -> [(Integer, Tile)]
readInput dat = map readTile $ splitOn "\n\n" dat
readTile :: String -> (Integer, Tile)
readTile dat = (tilenum, tiledat)
	where
		(tilenumstr:tilerows) = lines dat
		tilenum = read $ filter (/=':') $ (!!1) $ words $ tilenumstr
		tiledat = map (map (=='#')) tilerows

-- Various rotation functions (so hopefully I don't need to think about any of this in the actual algorithm)
rotatePointSize :: Integer -> Orientation -> Point -> Point
rotatePointSize size (o, f) (x, y) = (x'', y'')
	where
		x' = if f then size-x-1 else x
		y' = y
		(x'', y'') = rot o
		rot UpDir = (x', y')
		rot LeftDir = (y', size-x'-1)
		rot DownDir = (size-x'-1, size-y'-1)
		rot RightDir = (size-y'-1, x')
rotatePoint = rotatePointSize 10
rotateSide :: Orientation -> Orientation -> Orientation
rotateSide (o, f) (d, r) = (d'', r'')
	where
		d' = if f then flipDirX d else d
		r' = if f then not r else r
		d'' = rot o
		rot UpDir = d'
		rot LeftDir = rotDirLeft d'
		rot DownDir = reverseDirection d'
		rot RightDir = rotDirRight d'
		r'' = r'
-- left inverse of rotateSide
-- rotateSide (unrotateSide from to) from == to
unrotateSide (fromd, fromr) (tod, tor) = (o, f)
	where
		f = fromr /= tor
		fromd' = if f then flipDirX fromd else fromd
		o = mkrot fromd'
		mkrot UpDir = tod
		mkrot LeftDir = rotDirRight tod
		mkrot DownDir = reverseDirection tod
		mkrot RightDir = rotDirLeft tod
facingSide (o, f) = (reverseDirection o, not f)
-- inverse of rotation
-- rotateSide o (rotateside (invrot o) d) == d
-- rotateSide (invrot o) (rotateside o d) == d
invrot :: Orientation -> Orientation
invrot (LeftDir, False) = (RightDir, False)
invrot (RightDir, False) = (LeftDir, False)
invrot o = o

boolInt False = 0
boolInt True = 1
boolsInt = fromBaseN 2 . map boolInt

getEdges :: Tile -> [(Orientation, Integer)]
getEdges tile = edges
	where
		toprow = head tile
		bottomrow = last tile
		leftcol = map head tile
		rightcol = map last tile
		edges = [
			((UpDir, False), boolsInt toprow),
			((UpDir, True), boolsInt $ reverse toprow),
			((DownDir, False), boolsInt $ reverse bottomrow),
			((DownDir, True), boolsInt bottomrow),
			((LeftDir, False), boolsInt $ reverse leftcol),
			((LeftDir, True), boolsInt leftcol),
			((RightDir, False), boolsInt rightcol),
			((RightDir, True), boolsInt $ reverse rightcol)]

makeEdgeMap :: [(Integer, Tile)] -> M.Map (Integer, Orientation) Integer
makeEdgeMap tiles = M.fromList $ do
	(tileix, tile) <- tiles
	(o, edge) <- getEdges tile
	return ((tileix, o), edge)

makeEdgeRevMap :: [(Integer, Tile)] -> M.Map Integer [(Integer, Orientation)]
makeEdgeRevMap tiles = M.fromListWith (++) $ do
	(tileix, tile) <- tiles
	(o, edge) <- getEdges tile
	return (edge, [(tileix, o)])

solvePuzzle :: [(Integer, Tile)] -> Integer -> [[(Integer, Orientation)]]
solvePuzzle tiles size = head $ do
	let tileids = map fst tiles
	let tilemap = makeEdgeMap tiles
	let tilerevmap = makeEdgeRevMap tiles

	let getside s (x, xo) = tilemap M.! (x, rotateSide (invrot xo) s)
	let findside s e = [ (x, unrotateSide xo_ s) | (x, xo_) <- M.findWithDefault [] e tilerevmap ]
	let findmatch d x = findside (facingSide o) $ getside o x
		where o = (d, False)
	let ismatch d x y = getside o x == getside (facingSide o) y
		where o = (d, False)

	let doseen seen xs = [((x, xo), S.insert x seen) | (x, xo) <- xs, not $ S.member x seen]
	let firstcell seen = doseen seen [(x,o) | x <- tileids, o <- allOrients]
	let topcell (l, seen) = doseen seen $ findmatch RightDir l
	let leftcell seen t = doseen seen $ findmatch DownDir t
	let restcell (l, seen) t = doseen seen $ filter (ismatch DownDir t) $ findmatch RightDir l

	let extractseen xs = (map fst xs, snd $ last xs)
	let firstrow seen = map extractseen $ (iterateM (size-1) topcell) =<< firstcell seen
	let restrow ((prev:prevrest), seen) = map extractseen $ (\x->scanM restcell x prevrest) =<< leftcell seen prev
	let fullgrid seen = map extractseen $ (iterateM (size-1) restrow) =<< firstrow seen

	fst <$> fullgrid S.empty

calcPartA :: [[(Integer, Orientation)]] -> Integer
calcPartA result = foldl1 (*) $ map fst $ map ($result) [head.head, head.last, last.head, last.last]

assembleImage :: [(Integer, Tile)] -> [[(Integer, Orientation)]] -> Tile
assembleImage tiles result = do
	let tilemap = M.fromList tiles
	row <- result
	tilerow <- [1..8]
	return $ do
		(tileid, o) <- row
		let tile = tilemap M.! tileid
		tilecol <- [1..8]
		let (x, y) = rotatePoint (invrot o) (tilecol, tilerow)
		return $ tile `genericIndex` y `genericIndex` x

seamonster = [(x,y)|(y,row) <- enumerate $ lines "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   ", (x, c) <- enumerate row, c == '#']
seamonsterwidth = 1 + (maximum $ map fst seamonster)
seamonsterheight = 1 + (maximum $ map snd seamonster)

findSeaMonsters :: Array Point Bool -> Orientation -> [Point]
findSeaMonsters image o = do
	let size = 1 + (fst $ snd $ bounds image)
	x <- [0..size - seamonsterwidth]
	y <- [0..size - seamonsterheight]
	let checkpoint (mx, my) = image ! rotatePointSize size o (x + mx, y + my)
	guard $ all checkpoint seamonster
	return (x,y)

solvePartB :: [(Integer, Tile)] -> [[(Integer, Orientation)]] -> Integer
solvePartB tiles result = genericLength $ filter id $ elems newimage
	where
		imagelst = assembleImage tiles result
		size = genericLength imagelst
		image = listArrayLen2 imagelst
		(o, monsters) = maximumBy (compare `on` (genericLength.snd)) [(o, findSeaMonsters image o) | o <- allOrients]
		replacements = [(rotatePointSize size o (x+mx,y+my), False)|(mx,my) <- monsters, (x,y) <- seamonster]
		newimage = image // replacements

makeImage :: [(Integer, Tile)] -> [[(Integer, Orientation)]] -> String
makeImage tiles result = "P3\n" ++ (show size) ++ " " ++ (show size) ++ "\n256\n" ++ unwords (elems finalimage)
	where
		imagelst = assembleImage tiles result
		size = genericLength imagelst
		image = listArrayLen2 imagelst
		(o, monsters) = maximumBy (compare `on` (genericLength.snd)) [(o, findSeaMonsters image o) | o <- allOrients]
		imagecols = fmap (\c -> if c then "0 0 0" else "255 255 255") image
		replacements = [(rotatePointSize size o (x+mx,y+my), "254 1 1")|(mx,my) <- monsters, (x,y) <- seamonster]
		newimage = imagecols // replacements
		finalimage = ixmap (0,size*size-1) (\n -> rotatePointSize size o $ swap (divMod n size)) newimage

tests :: IO ()
tests = do
	check $ calcPartA testsoln == 20899048083289
	check $ solvePartB testdata testsoln == 273
	writeFile "20_test.ppm" $ makeImage testdata testsoln
	where
		testdata = readInput "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###\n\nTile 1951:\n#.##...##.\n#.####...#\n.....#..##\n#...######\n.##.#....#\n.###.#####\n###.##.##.\n.###....#.\n..#.#..#.#\n#...##.#..\n\nTile 1171:\n####...##.\n#..##.#..#\n##.#..#.#.\n.###.####.\n..###.####\n.##....##.\n.#...####.\n#.##.####.\n####..#...\n.....##...\n\nTile 1427:\n###.##.#..\n.#..#.##..\n.#.##.#..#\n#.#.#.##.#\n....#...##\n...##..##.\n...#.#####\n.#.####.#.\n..#..###.#\n..##.#..#.\n\nTile 1489:\n##.#.#....\n..##...#..\n.##..##...\n..#...#...\n#####...#.\n#..#.#.#.#\n...#.#.#..\n##.#...##.\n..##.##.##\n###.##.#..\n\nTile 2473:\n#....####.\n#..#.##...\n#.##..#...\n######.#.#\n.#...#.#.#\n.#########\n.###.#..#.\n########.#\n##...##.#.\n..###.#.#.\n\nTile 2971:\n..#.#....#\n#...###...\n#.#.###...\n##.##..#..\n.#####..##\n.#..####.#\n#..#.#..#.\n..####.###\n..#.#.###.\n...#.#.#.#\n\nTile 2729:\n...#.#.#.#\n####.#....\n..#.#.....\n....#..#.#\n.##..##.#.\n.#.####...\n####.#.#..\n##.####...\n##..#.##..\n#.##...##.\n\nTile 3079:\n#.#.#####.\n.#..######\n..#.......\n######....\n####.#..#.\n.#...#.##.\n#.#####.##\n..#.###...\n..#.......\n..#.###..."
		testsoln = solvePuzzle testdata 3
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	tiles <- getInput
	let solution = solvePuzzle tiles 12
	print $ calcPartA solution
	print $ solvePartB tiles solution
	writeFile "20.ppm" $ makeImage tiles solution
