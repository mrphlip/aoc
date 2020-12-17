{-# OPTIONS_GHC -Wno-tabs #-}
module Range (Ranges, fromPairs, inRange, union, intersection, Range.subtract) where

import Data.List (sortBy)
import Data.Function
import qualified Data.Map.Strict as M

type Ranges i = M.Map i i

fromPairs :: (Num i, Ord i) => [(i, i)] -> Ranges i
fromPairs ranges = M.fromList groupedranges
	where
		sortedranges = sortBy (compare `on` fst) ranges
		groupedranges = group_start sortedranges
		group_start [] = []
		group_start (x:rest) = group_cont x rest
		group_cont x [] = [x]
		group_cont (a,b) ((a',b'):rest)
			| a' <= b = group_cont (a, max b b') rest
			| otherwise = (a,b) : group_cont (a', b') rest

inRange :: (Num i, Ord i) => i -> Ranges i -> Bool
inRange x ranges = case M.lookupLE x ranges of
	Just (_, endrange) -> x <= endrange
	Nothing -> False

union :: (Num i, Ord i) => Ranges i -> Ranges i -> Ranges i
union a b = fromPairs $ M.assocs a ++ M.assocs b

intersection :: (Num i, Ord i) => Ranges i -> Ranges i -> Ranges i
intersection a b = fromPairs $ getPairs (M.assocs a) (M.assocs b)
	where
		getPairs [] _ = []
		getPairs _ [] = []
		getPairs as@((amin, amax):arest) bs@((bmin, bmax):brest)
			| amin > bmin = getPairs bs as
			| amax < bmin = getPairs bs arest
			| amax <= bmax = (bmin, amax) : getPairs bs arest
			| otherwise = (bmin, bmax) : getPairs as brest

subtract :: (Num i, Ord i, Enum i) => Ranges i -> Ranges i -> Ranges i
subtract a b = fromPairs $ getPairs (M.assocs a) (M.assocs b)
	where
		getPairs [] _ = []
		getPairs as [] = as
		getPairs as@((amin, amax):arest) bs@((bmin, bmax):brest)
			| amax < bmin = (amin, amax) : getPairs arest bs
			| bmax < amin = getPairs as brest
			-- TODO: the use of succ/pred implies a discrete datatype...
			-- would be nice to have proper open/closed range endpoints
			-- so we can support reals
			| amin < bmin && amax > bmax = (amin, pred bmin) : getPairs ((succ bmax,amax):arest) brest
			| amin < bmin && amax <= bmax = (amin, pred bmin) : getPairs arest bs
			| amin >= bmin && amax > bmax = getPairs ((succ bmax,amax):arest) brest
			| amin >= bmin && amax <= bmax = getPairs arest bs
