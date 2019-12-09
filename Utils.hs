module Utils (split, listArrayLen, enumerate, chunk) where

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
