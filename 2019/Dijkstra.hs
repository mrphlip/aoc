{-# OPTIONS_GHC -Wno-tabs #-}
module Dijkstra(DistMap, buildDistMap, findNearest, buildDistMapSquare, buildDistMapSquareExpand, findNearestSquare, findNearestSquareExpand, makeArgsSquare, makeArgsSquareExpand) where

import Data.Ix
import Data.Array
import Data.Maybe
import Direction
import Utils

-- DistMap is an array of (distance to start point, direction from previous point, location of previous point)
type DistMap i n d = Array i (Maybe (n, Maybe d, i))

-- mapbounds == boundary of map to build
-- startnode == location in map to start at
-- neighbours == function from location to list of (neighbour location, direction to neighbour, distance to neighbour)
-- istarget == stop mapping once we find the route to any target - can be "const False" to route the entire map
-- returns a DistMap, and the location of the target point found
buildDistMap :: (Ix i, Num n, Ord n) => (i, i) -> i -> (i -> [(i, d, n)]) -> (i -> Bool) -> (DistMap i n d, Maybe i)
buildDistMap mapbounds startnode neighbours istarget = iterfunc initdistmap
	where
		initdistmap = listArray mapbounds (repeat Nothing) // [ (startnode, Just (0, Nothing, startnode)) ]
		iterfunc distmap = if shouldStop then (distmap, Nothing) else continue
			where
				-- from, to, direction, distance from start point
				points = [(p, p', d, ((\(Just (x,_,_))->x) $ distmap ! p) + n) |
					p <- indices distmap,
					isJust $ distmap ! p,
					(p', d, n) <- neighbours p,
					isNothing $ distmap ! p' ]
				shouldStop = null points
				closestDist = minimum [ n | (_,_,_,n) <- points ]
				filtPoints = [ x | x@(_,_,_,n) <- points, n == closestDist ]
				newMap = distmap // [ (p', Just (n, Just d, p)) | (p,p',d,n) <- filtPoints ]
				targetPoints = [ p' | (_,p',_,_) <- filtPoints, istarget p' ]
				continue = case targetPoints of
					(p':_) -> (newMap, Just p')
					_ -> iterfunc newMap

-- same parameters as buildDistMap
-- returns a sequence of directions leading fromt he startnode to the target, if one is found
-- or Nothing, if none is found
findNearest :: (Ix i, Num n, Ord n) => (i, i) -> i -> (i -> [(i, d, n)]) -> (i -> Bool) -> Maybe ([d], n)
findNearest mapbounds startnode neighbours istarget = result
	where
		result = case buildDistMap mapbounds startnode neighbours istarget of
			(finalMap, Just targetLoc) -> Just (reverse $ tracePoint finalMap targetLoc, (\(dist,_,_)->dist) $ fromJust $ finalMap ! targetLoc)
			(_, Nothing) -> Nothing
		tracePoint finalMap p
			| isNothing dir = []
			| otherwise = fromJust dir : tracePoint finalMap prev
			where Just (_, dir, prev) = finalMap ! p

makeArgsSquare maze allowed target = (mapbounds, neighbours, istarget)
	where
		mapbounds = bounds maze
		neighbours p = [ (p', d, 1) | d <- directions, let p' = step d p, inRange mapbounds p', maze ! p' `elem` allowed ]
		istarget p = (maze ! p) `elem` target

makeArgsSquareExpand maze allowed target expandby expandwith = (mapbounds, neighbours, istarget)
	where
		((minx,miny),(maxx,maxy)) = bounds maze
		mapbounds = ((minx-expandby,miny-expandby),(maxx+expandby,maxy+expandby))
		neighbours p = [ (p', d, 1) | d <- directions, let p' = step d p, inRange mapbounds p', getExpand p' expandwith maze `elem` allowed ]
		istarget p = getExpand p expandwith maze `elem` target

buildDistMapSquare :: (Eq x) => Array (Integer, Integer) x -> (Integer, Integer) -> [x] -> [x] -> (DistMap (Integer, Integer) Integer Direction, Maybe (Integer, Integer))
buildDistMapSquare maze startnode allowed target = buildDistMap mapbounds startnode neighbours istarget
	where (mapbounds, neighbours, istarget) = makeArgsSquare maze allowed target

buildDistMapSquareExpand :: (Eq x) => Array (Integer, Integer) x -> (Integer, Integer) -> [x] -> [x] -> Integer -> x -> (DistMap (Integer, Integer) Integer Direction, Maybe (Integer, Integer))
buildDistMapSquareExpand maze startnode allowed target expandby expandwith = buildDistMap mapbounds startnode neighbours istarget
	where (mapbounds, neighbours, istarget) = makeArgsSquareExpand maze allowed target expandby expandwith

findNearestSquare :: (Eq x) => Array (Integer, Integer) x -> (Integer, Integer) -> [x] -> [x] -> Maybe ([Direction], Integer)
findNearestSquare maze startnode allowed target = findNearest mapbounds startnode neighbours istarget
	where (mapbounds, neighbours, istarget) = makeArgsSquare maze allowed target

findNearestSquareExpand :: (Eq x) => Array (Integer, Integer) x -> (Integer, Integer) -> [x] -> [x] -> Integer -> x -> Maybe ([Direction], Integer)
findNearestSquareExpand maze startnode allowed target expandby expandwith = findNearest mapbounds startnode neighbours istarget
	where (mapbounds, neighbours, istarget) = makeArgsSquareExpand maze allowed target expandby expandwith

