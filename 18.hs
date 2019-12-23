{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Array
import Data.List
import Data.Char
import Data.Maybe
import Data.Function
import Data.Bits
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Utils
import Dijkstra
import Debug.Trace

type Key = Char
data Cell = Floor | Wall | Key Key | Door Key deriving (Eq, Show, Read)
type Point = (Integer, Integer)
type Maze = Array Point Cell
type State = (Maze, Point, M.Map Key Point, S.Set Key) -- maze, current location, key locations, keys acquired

getInput :: IO State
getInput = do
	dat <- readFile "18.txt"
	return $ readMaze dat

readCell :: Char -> Cell
readCell '.' = Floor
readCell '@' = Floor
readCell '#' = Wall
readCell c
	| isUpper c = Door c
	| isLower c = Key $ toUpper c

readMaze :: String -> State
readMaze s = (array bounds cells, startloc, keylocs, S.empty)
	where
		width = genericLength $ head rows
		height = genericLength rows
		bounds = ((0, 0), (width-1, height-1))

		rows = lines s
		cells = [ ((x,y), readCell c) | (y, row) <- enumerate $ rows, (x, c) <- enumerate row ]

		startloc = head [ (x,y) | (y, row) <- enumerate $ rows, (x, c) <- enumerate row, c == '@' ]
		keylocs = M.fromList [ (toUpper c, (x, y)) | (y, row) <- enumerate $ rows, (x, c) <- enumerate row, isLower c ]

reachableKeys :: State -> [(Key, Point, Integer)]
reachableKeys (maze, loc, keylocs, keys) = targets
	where
		allowed = [Floor] ++ [Key c | c <- M.keys keylocs] ++ [Door c | c <- S.toList keys]
		(distmap, _) = buildDistMapSquare maze loc allowed []
		targets = do
			(c, keyloc) <- M.assocs keylocs
			guard $ not $ S.member c keys
			let dist = distmap ! keyloc
			guard $ isJust $ dist
			let Just (d, _, _) = dist
			return (c, keyloc, d)

-- just brute-force search every key order to see what's the fastest
-- simple but slow
fastestRoute_brute :: State -> Maybe ([Key], Integer)
fastestRoute_brute state@(_,_,_,k) = if null options then Nothing else Just $ minimumBy (compare `on` snd) $ options
	where options = do
		(c, loc, d) <- reachableKeys state
		let (maze, _, keylocs, keys) = state
		let newkeys = S.insert c keys
		if newkeys == M.keysSet keylocs
			then return ([c], d)
			else do
				(rest, restdist) <- maybeToList $ fastestRoute_brute (maze, loc, keylocs, newkeys)
				return (c:rest, d+restdist)

-- do a second-level dijkstra over the key-collection statespace graph
-- somewhat faster but requires an array with 2**26 * 26 nodes for the distmap, which is way too much
fastestRoute_dijkstra :: State -> Maybe ([Key], Integer)
fastestRoute_dijkstra (maze, startloc, keylocs, _) = findNearest bounds (0,0) neighbours istarget
	where
		-- our "location" in the statespace graph is the set of keys we collected
		-- and the identity of the latest-collected key
		keycount = M.size keylocs
		allkeys = bit keycount - 1
		bounds = ((0, 0), (allkeys, keycount - 1)) :: ((Integer,Int),(Integer,Int))

		keyix c = ord c - 65
		keyname n = chr (n + 65)
		tokeyset k = S.fromList [key | key <- M.keys keylocs, k .&. bit (keyix key) /= 0]
		fromkeyset ks = foldr1 (.|.) [bit (keyix k) | k <- S.elems ks]

		currentloc :: (Integer, Int) -> Point
		currentloc (0, _) = startloc -- if we have no keys, we're at the starting location
		currentloc (_, n) = keylocs M.! keyname n -- otherwise we're at the latest-collected key
		istarget :: (Integer, Int) -> Bool
		istarget (k, _) = k == allkeys
		neighbours :: (Integer, Int) -> [((Integer, Int), Key, Integer)]
		neighbours_ (k, n) = do
			(c, _, d) <- reachableKeys (maze, currentloc (k, n), keylocs, tokeyset k)
			return ((k .|. 1 `shift` keyix c, keyix c), c, d)
		-- Be strict here to ensure that the distmap inside reachableKeys can be freed in a timely manner
		neighbours p = let n = neighbours p in length n `seq` n

keyroutes :: Maze -> Array Key Point -> M.Map Key Point -> Array (Key,Key) (Maybe (Integer, S.Set Key))
keyroutes maze startlocs keylocs = routes
	where
		keycount = M.size keylocs
		lastkey = chr (64 + keycount)
		(firststartloc, _) = bounds startlocs
		routesbounds = ((firststartloc, 'A'), (lastkey, lastkey))

		allowed = [Floor] ++ [Key c | c <- M.keys keylocs] ++ [Door c | c <- M.keys keylocs]
		tracepoint distmap p = case distmap ! p of
			Just (_, Nothing, _) -> []
			Just (_, Just _, p') -> case maze ! p' of
				Door k -> k : tracepoint distmap p'
				_ -> tracepoint distmap p'
		routesdata = startroutesdata ++ keyroutesdata
		startroutesdata = do
			(sk, startloc) <- assocs startlocs
			let (distmap, _) = buildDistMapSquare maze startloc allowed []
			(k, keyloc) <- M.assocs keylocs
			let dist = distmap ! keyloc
			if isJust dist
				then do
					let Just (d, _, _) = dist
					return ((sk, k), Just (d, S.fromList $ tracepoint distmap keyloc))
				else return ((sk, k), Nothing)
		keyroutesdata = do
			(k1, keyloc1) <- M.assocs keylocs
			let (distmap, _) = buildDistMapSquare maze keyloc1 allowed []
			(k2, keyloc2) <- M.assocs keylocs
			let dist = distmap ! keyloc2
			if isJust dist
				then do
					let Just (d, _, _) = dist
					return ((k1, k2), Just (d, S.fromList $ tracepoint distmap keyloc2))
				else return ((k1, k2), Nothing)
		routes = array routesbounds routesdata

-- brute force again, but precalculate the best route from each key to each key, and which doors the route passes through
-- only really valid if the maze is a tree (which it appears to be)
fastestRoute_tree :: State -> Maybe ([Key], Integer)
fastestRoute_tree (maze, startloc, keylocs, keyset) = fastest '@' keyset
	where
		startlocs = listArray ('@','@') [startloc]
		routes = keyroutes maze startlocs keylocs

		fastest :: Key -> S.Set Key -> Maybe ([Key], Integer)
		fastest lastkey keyset = if null options then Nothing else Just $ minimumBy (compare `on` snd) $ options
			where options = do
				nextkey <- M.keys keylocs
				guard $ not $ nextkey `elem` keyset
				let route = routes ! (lastkey, nextkey)
				guard $ isJust route
				let Just (dist, keyreq) = route
				guard $ keyreq `S.isSubsetOf` keyset
				let newkeys = nextkey `S.insert` keyset
				if newkeys == M.keysSet keylocs
					then return ([nextkey], dist)
					else do
						(rest, restdist) <- maybeToList $ fastest nextkey newkeys
						return (nextkey:rest, dist+restdist)

-- same again, but attempt to memoise... but instead of using an Array like in the
-- Dijkstra attempt, which tried to allocate 2**26 * 26 cells, use a sparse map that
-- we have to update as we go... which we then need to pass through all the recursive
-- calls, because Haskell
-- None of the lazy memoisation techniques I could find in Google work on a domain this
-- large and sparse
type Memo = M.Map (S.Set Key, Key) (Maybe ([Key], Integer))
fastestRoute_tree_dyn :: State -> Maybe ([Key], Integer)
fastestRoute_tree_dyn (maze, startloc, keylocs, keyset) = fst $ fastest '@' keyset initmemo
	where
		startlocs = listArray ('@','@') [startloc]
		routes = keyroutes maze startlocs keylocs

		initmemo :: Memo
		initmemo = M.empty

		fastest :: Key -> S.Set Key -> Memo -> (Maybe ([Key], Integer), Memo)
		fastest lastkey keyset memo
			| (keyset, lastkey) `M.member` memo = (memo M.! (keyset, lastkey), memo)
			| otherwise = let (res, memo') = calcFastest lastkey keyset memo in (res, M.insert (keyset, lastkey) res memo')
		calcFastest :: Key -> S.Set Key -> Memo -> (Maybe ([Key], Integer), Memo)
		calcFastest lastkey keyset memo = (result, finalmemo)
			where
				result = if null options then Nothing else Just $ minimumBy (compare `on` snd) $ options
				(options, finalmemo) = getoptions (M.keys keylocs) memo
				getoptions [] memo = ([], memo)
				getoptions (nextkey:restkeys) memo = case makeoption nextkey memo of
					Nothing -> getoptions restkeys memo
					Just (option, memo') -> let (rest, memo'') = getoptions restkeys memo' in (option:rest, memo'')
				makeoption nextkey memo
					| nextkey `elem` keyset = Nothing
					| isNothing route = Nothing
					| not $ keyreq `S.isSubsetOf` keyset = Nothing
					| newkeys == M.keysSet keylocs = Just (([nextkey], dist), memo)
					| otherwise = case continue of
							Nothing -> Nothing
							Just (restroute, restdist) -> Just ((nextkey:restroute, dist+restdist), memo')
					where
						route = routes ! (lastkey, nextkey)
						Just (dist, keyreq) = route
						newkeys = nextkey `S.insert` keyset
						(continue, memo') = fastest nextkey newkeys memo

type MemoMulti = M.Map (S.Set Key, Key, Key, Key, Key) (Maybe ([Key], Integer))
fastestRouteMulti :: State -> Maybe ([Key], Integer)
fastestRouteMulti (origmaze, (startx, starty), keylocs, keyset) = fst $ fastest '=' '>' '?' '@' keyset initmemo
	where
		maze = origmaze // [((startx+x, starty+y), Wall) | (x,y) <- [(0,0),(1,0),(-1,0),(0,1),(0,-1)]]
		startlocs = listArray ('=', '@') [(startx+x, starty+y) | x <- [-1, 1], y <- [-1, 1]]
		routes = keyroutes maze startlocs keylocs

		initmemo :: MemoMulti
		initmemo = M.empty

		fastest :: Key -> Key -> Key -> Key -> S.Set Key -> MemoMulti -> (Maybe ([Key], Integer), MemoMulti)
		fastest lk1 lk2 lk3 lk4 keyset memo
			| (keyset, lk1, lk2, lk3, lk4) `M.member` memo = (memo M.! (keyset, lk1, lk2, lk3, lk4), memo)
			| otherwise = let (res, memo') = calcFastest lk1 lk2 lk3 lk4 keyset memo in (res, M.insert (keyset, lk1, lk2, lk3, lk4) res memo')
		calcFastest :: Key -> Key -> Key -> Key -> S.Set Key -> MemoMulti -> (Maybe ([Key], Integer), MemoMulti)
		calcFastest lk1 lk2 lk3 lk4 keyset memo = (result, finalmemo)
			where
				result = if null options then Nothing else Just $ minimumBy (compare `on` snd) $ options
				(options, finalmemo) = getoptions (M.keys keylocs) memo
				getoptions [] memo = ([], memo)
				getoptions (nextkey:restkeys) memo = (option ++ rest, memo'')
					where
						(option, memo') = makeoption nextkey memo
						(rest, memo'') = getoptions restkeys memo'
				makeoption nextkey memo = (catMaybes [res1, res2, res3, res4], memo'''')
					where
						(res1, memo') = makeoptionfrom 1 lk1 nextkey memo
						(res2, memo'') = makeoptionfrom 2 lk2 nextkey memo'
						(res3, memo''') = makeoptionfrom 3 lk3 nextkey memo''
						(res4, memo'''') = makeoptionfrom 4 lk4 nextkey memo'''
				makeoptionfrom ix lastkey nextkey memo
					| nextkey `elem` keyset = (Nothing, memo)
					| isNothing route = (Nothing, memo)
					| not $ keyreq `S.isSubsetOf` keyset = (Nothing, memo)
					| newkeys == M.keysSet keylocs = (Just ([nextkey], dist), memo)
					| otherwise = case continue of
							Nothing -> (Nothing, memo)
							Just (restroute, restdist) -> (Just (nextkey:restroute, dist+restdist), memo')
					where
						route = routes ! (lastkey, nextkey)
						Just (dist, keyreq) = route
						newkeys = nextkey `S.insert` keyset
						update 1 x a b c d = (x, b, c, d)
						update 2 x a b c d = (a, x, c, d)
						update 3 x a b c d = (a, b, x, d)
						update 4 x a b c d = (a, b, c, x)
						(nk1, nk2, nk3, nk4) = update ix nextkey lk1 lk2 lk3 lk4
						(continue, memo') = fastest nk1 nk2 nk3 nk4 newkeys memo

showRoutes :: Array (Key,Key) (Maybe (Integer, S.Set Key)) -> String
showRoutes routes = unlines $ [ show (k1, k2, dist, keyreq) | ((k1, k2), val) <- assocs routes, isJust val, let Just (dist, keyreq) = val ]

--fastestRoute = fastestRoute_brute
--fastestRoute = fastestRoute_dijkstra
--fastestRoute = fastestRoute_tree
fastestRoute = fastestRoute_tree_dyn

tests = do
	testMaze "#########\n#b.A.@.a#\n#########" ("AB", 8)
	testMaze "########################\n#f.D.E.e.C.b.A.@.a.B.c.#\n######################.#\n#d.....................#\n########################" ("ABCDEF", 86)
	testMaze "########################\n#...............b.C.D.f#\n#.######################\n#.....@.a.B.c.d.A.e.F.g#\n########################" ("BACDEFG", 132)
	testMaze "#################\n#i.G..c...e..H.p#\n########.########\n#j.A..b...f..D.o#\n########@########\n#k.E..a...g..B.n#\n########.########\n#l.F..d...h..C.m#\n#################" ("AFBJGNHDLOEPCIKM", 136)
	testMaze "########################\n#@..............ac.GI.b#\n###d#e#f################\n###A#B#C################\n###g#h#i################\n########################" ("ACFIDGBEH", 81)

	testMazeMulti "#######\n#a.#Cd#\n##...##\n##.@.##\n##...##\n#cB#Ab#\n#######" ("ABCD", 8)
	testMazeMulti "###############\n#d.ABC.#.....a#\n######...######\n######.@.######\n######...######\n#b.....#.....c#\n###############" ("ABCD", 24)
	testMazeMulti "#############\n#DcBa.#.GhKl#\n#.###...#I###\n#e#d#.@.#j#k#\n###C#...###J#\n#fEbA.#.FgHi#\n#############" ("ABCDEFGHIJKL", 32)
	testMazeMulti "#############\n#g#f.D#..h#l#\n#F###e#E###.#\n#dCba...BcIJ#\n#####.@.#####\n#nK.L...G...#\n#M###N#H###.#\n#o#m..#i#jk.#\n#############" ("EHIABCDFGKJLNMO", 72)
	where
		testMaze s (_, d) = do
			let maze = readMaze s
			--test $ (snd $ fromJust $ fastestRoute_brute maze) == d
			--test $ (snd $ fromJust $ fastestRoute_dijkstra maze) == d
			--test $ (snd $ fromJust $ fastestRoute_tree maze) == d
			test $ (snd $ fromJust $ fastestRoute maze) == d
		testMazeMulti s (_, d) = do
			let maze = readMaze s
			test $ (snd $ fromJust $ fastestRouteMulti maze) == d

main = do
	maze <- getInput
	print $ fastestRoute maze
	print $ fastestRouteMulti maze
