{-# OPTIONS_GHC -Wno-tabs #-}
module Modulo (Modulo, modulo, modulus) where

import Data.List
import Utils (extendedGcd)

data Modulo x = !x `Mod` !x deriving Eq

infix 5 `modulo`
n `modulo` m = (n `mod` m) `Mod` m

modulusMismatch (_ `Mod` m) (_ `Mod` m') = m /= m'
modulusMismatch3 (_ `Mod` m) (_ `Mod` m') (_ `Mod` m'') = m /= m' || m' /= m''
modulusMismatchList [] = False
modulusMismatchList (x:xs) = any (modulusMismatch x) xs

instance (Integral x, Show x) => Show (Modulo x) where
	showsPrec p (n `Mod` m) s =
		(showParen (p >= 9) $
		showsPrec 11 n .
		showString " `modulo` " .
		showsPrec 11 m) s

instance (Integral x) => Enum (Modulo x) where
	fromEnum (n `Mod` m) = fromEnum $ toInteger (m*(m-1)`div`2 + n)
	toEnum i = let (m,offs) = last $ takeWhile ((<=i).snd) triangles in fromIntegral (i - offs) `Mod` fromIntegral m
		where triangles = map(\x->(x,x*(x-1)`div`2)) [1..]
	succ (n `Mod` m) = (n+1) `modulo` m
	pred (n `Mod` m) = (n-1) `modulo` m
	enumFrom (n `Mod` m) = genericDrop n $ cycle $ map (`Mod` m) $ [0..(m-1)]
	enumFromTo x y = enumFromThenTo x (succ x) y
	-- Note... this will give an infinite list if it never reaches the final value
	-- eg [0 `modulo` 10, 2 `modulo` 10 .. 9 `modulo` 10]
	-- I think this is the Right Thing for it to do... I think we want it to loop properly
	-- if given eg [0 `modulo` 7, 2 `modulo` 7 .. 5 `modulo` 7]
	-- which will give [0,2,4,6,1,3,5]
	enumFromThenTo x y z
		| modulusMismatch3 x y z = error "enumFromThenTo: mismatched moduli"
		| x == z = [x]
		| otherwise = let (xn `Mod` m) = x; (yn `Mod` _) = y in x:enumFromThenTo y ((yn * 2 - xn) `modulo` m) z

instance (Integral x) => Num (Modulo x) where
	(n1 `Mod` m) + (n2 `Mod` m')
		| m /= m' = error "(+): mismatched moduli"
		| otherwise = (n1 + n2) `modulo` m
	(n1 `Mod` m) - (n2 `Mod` m')
		| m /= m' = error "(-): mismatched moduli"
		| otherwise = (n1 - n2) `modulo` m
	(n1 `Mod` m) * (n2 `Mod` m')
		| m /= m' = error "(*): mismatched moduli"
		| otherwise = (n1 * n2) `modulo` m
	negate (n `Mod` m) = (-n) `modulo` m
	abs = id
	signum (n `Mod` _) = if n == 0 then 0 else 1
	fromInteger = error "fromInteger: cannot guess modulus"

-- They're not really ordered, but being Ord is still useful
-- for putting these in sets, etc
instance (Integral x) => Ord (Modulo x) where
	compare (a `Mod` _) (b `Mod` _) = compare a b

instance (Integral x) => Real (Modulo x) where
	toRational (n `Mod` _) = toRational n

-- It's not really fractional, I know, but it fits the definition to an extent
instance (Integral x) => Fractional (Modulo x) where
	fromRational = error "fromRational: cannot guess modulus"
	recip (n `Mod` m)
		| m == 0 = error "divide by zero"
		| c /= 1 = error "recip: value not coprime to modulus"
		| otherwise = a `modulo` m
			where (a, b, c) = extendedGcd n m

instance (Integral x) => Integral (Modulo x) where
	toInteger (n `Mod` _) = toInteger n
	a `divMod` b = (a/b, 0)
	a `div` b = a/b
	a `mod` b = 0
	quotRem = divMod
	quot = div
	rem = mod

-- To extract the number part, use toInteger
modulus (_ `Mod` m) = m
