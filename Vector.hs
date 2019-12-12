module Vector (Vector(..), Vector2(Vector2), Vector3(Vector3), crossProduct, tupleVec2, tupleVec3, vecTuple2, vecTuple3, vecSum) where

import Data.List

data Vector2 a = Vector2 a a deriving Eq
data Vector3 a = Vector3 a a a deriving Eq

instance Read a => Read (Vector2 a) where
	readsPrec n s = do
		(v,rest) <- readsPrec n s
		return (tupleVec2 v, rest)
instance Show a => Show (Vector2 a) where
	showsPrec n v = showsPrec n $ vecTuple2 v

instance Read a => Read (Vector3 a) where
	readsPrec n s = do
		(v,rest) <- readsPrec n s
		return (tupleVec3 v, rest)
instance Show a => Show (Vector3 a) where
	showsPrec n v = showsPrec n $ vecTuple3 v

class Vector v where
	(.+) :: (Num a) => v a -> v a -> v a
	(.-) :: (Num a) => v a -> v a -> v a
	negate :: (Num a) => v a -> v a
	(.*) :: (Num a) => v a -> a -> v a
	(./) :: (Fractional a) => v a -> a -> v a
	dotProduct :: (Num a) => v a -> v a -> a
	zero :: (Num a) => v a

instance Vector Vector2 where
	Vector2 ax ay .+ Vector2 bx by = Vector2 (ax + bx) (ay + by)
	Vector2 ax ay .- Vector2 bx by = Vector2 (ax - bx) (ay - by)
	negate (Vector2 x y) = Vector2 (Prelude.negate x) (Prelude.negate y)
	Vector2 ax ay .* s = Vector2 (ax * s) (ay * s)
	Vector2 ax ay ./ s = Vector2 (ax / s) (ay / s)
	Vector2 ax ay `dotProduct` Vector2 bx by = (ax * bx) + (ay * by)
	zero = Vector2 0 0

instance Vector Vector3 where
	Vector3 ax ay az .+ Vector3 bx by bz = Vector3 (ax + bx) (ay + by) (az + bz)
	Vector3 ax ay az .- Vector3 bx by bz = Vector3 (ax - bx) (ay - by) (az - bz)
	negate (Vector3 x y z) = Vector3 (Prelude.negate x) (Prelude.negate y) (Prelude.negate z)
	Vector3 ax ay az .* s = Vector3 (ax * s) (ay * s) (az * s)
	Vector3 ax ay az ./ s = Vector3 (ax / s) (ay / s) (az / s)
	Vector3 ax ay az `dotProduct` Vector3 bx by bz = (ax * bx) + (ay * by) + (az * bz)
	zero = Vector3 0 0 0

crossProduct :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
Vector3 ax ay az `crossProduct` Vector3 bx by bz = Vector3 x y z
	where
		x = ay * bz - az * by
		y = az * bx - ax * bz
		z = ax * by - ay * bx

tupleVec2 (x, y) = Vector2 x y
tupleVec3 (x, y, z) = Vector3 x y z
vecTuple2 (Vector2 x y) = (x, y)
vecTuple3 (Vector3 x y z) = (x, y, z)

vecSum :: (Vector v, Num a) => [v a] -> v a
vecSum = foldr1 (.+)
