module Utils (split, listArrayLen, enumerate, chunk, inBounds, changeBounds, getExpand, setExpand, ExpandIx) where

import Data.Array
import Data.List

-- Not entirely sure why this isn't a builtin...
split :: Char -> String -> [String]
split c s
	| null s = [""]
	| otherwise = let (first, rest) = break (==c) s in first : (if null rest then [] else split c (tail rest))

listArrayLen :: (Num a, Ix a) => [b] -> Array a b
listArrayLen xs = listArray (0, genericLength xs - 1) xs

enumerate :: (Num n) => [a] -> [(n, a)]
enumerate = enumerateFrom 0
enumerateFrom :: (Num n) => n -> [a] -> [(n, a)]
enumerateFrom _ [] = []
enumerateFrom n (x:xs) = (n,x) : enumerateFrom (n+1) xs

chunk :: (Integral i) => i -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = front : chunk n back
	where (front, back) = genericSplitAt n xs

inBounds :: (Ix i) => Array i a -> i -> Bool
inBounds arr ix = inRange (bounds arr) ix

changeBounds :: (Ix i) => (i,i) -> a -> Array i a -> Array i a
changeBounds newrange def arr = cleared
	where
		oldrange = bounds arr
		expanded = ixmap newrange ixmapfunc arr
		ixmapfunc ix
			| inRange oldrange ix = ix
			| otherwise = fst oldrange
		cleared = expanded // [ (ix,def) | ix <- range newrange, not $ inRange oldrange ix ]

getExpand :: (Ix i) => i -> a -> Array i a -> a
getExpand ix def arr
	| inBounds arr ix = arr ! ix
	| otherwise = def

setExpand :: (ExpandIx i) => i -> a -> a -> Array i a -> Array i a
setExpand ix val def arr
	| inBounds arr ix = arr // [(ix, val)]
	| otherwise = changeBounds (expandBounds (bounds arr) ix) def arr // [(ix, val)]

class (Ix i) => ExpandIx i where
	expandBounds :: (i,i) -> i -> (i,i)

instance ExpandIx Integer where
	expandBounds (a,b) v = (min a v, max b v)

instance (ExpandIx a, ExpandIx b) => ExpandIx (a, b) where
	expandBounds ((ax,ay), (bx,by)) (x,y) = ((ax',ay'), (bx',by'))
		where
			(ax', bx') = expandBounds (ax,bx) x
			(ay', by') = expandBounds (ay,by) y
