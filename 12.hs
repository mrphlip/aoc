import Data.List
import Vector
import Utils

type Planet = (Vector3 Integer, Vector3 Integer) -- Position, Velocity

gravity1 :: Integer -> Integer -> Integer
gravity1 a b
	| a < b = 1
	| a > b = -1
	| a == b = 0

gravity3 :: Vector3 Integer -> Vector3 Integer -> Vector3 Integer
gravity3 (Vector3 ax ay az) (Vector3 bx by bz) = (Vector3 x y z)
	where
		x = gravity1 ax bx
		y = gravity1 ay by
		z = gravity1 az bz

step :: [Planet] -> [Planet]
step planets = afterVelocity
	where
		gravity = [ vecSum [ gravity3 p p' | (p',_) <- planets ] | (p,_) <- planets ]
		afterGravity = [ (p, v .+ g) | ((p, v), g) <- zip planets gravity ]
		afterVelocity = [ (p .+ v, v) | (p, v) <- afterGravity ]

energy :: [Planet] -> Integer
energy planets = sum $ map calcEnergy planets
	where
		calcEnergy (p, v) = calcPot p * calcKin v
		calcPot (Vector3 x y z) = abs x + abs y + abs z
		calcKin (Vector3 x y z) = abs x + abs y + abs z

tests :: IO ()
tests = do
	let init = [
			(Vector3 (-1) 0 2, zero),
			(Vector3 2 (-10) (-7), zero),
			(Vector3 4 (-8) 8, zero),
			(Vector3 3 5 (-1), zero)
		]
	let steps = iterate step init
	test $ steps !! 1 == [
			(Vector3 2 (-1) 1, Vector3 3 (-1) (-1)),
			(Vector3 3 (-7) (-4), Vector3 1 3 3),
			(Vector3 1 (-7) 5, Vector3 (-3) 1 (-3)),
			(Vector3 2 2 0, Vector3 (-1) (-3) 1)
		]
	test $ steps !! 2 == [
			(Vector3 5 (-3) (-1), Vector3 3 (-2) (-2)),
			(Vector3 1 (-2) 2, Vector3 (-2) 5 6),
			(Vector3 1 (-4) (-1), Vector3 0 3 (-6)),
			(Vector3 1 (-4) 2, Vector3 (-1) (-6) 2)
		]
	test $ steps !! 3 == [
			(Vector3 5 (-6) (-1), Vector3 0 (-3) 0),
			(Vector3 0 0 6, Vector3 (-1) 2 4),
			(Vector3 2 1 (-5), Vector3 1 5 (-4)),
			(Vector3 1 (-8) 2, Vector3 0 (-4) 0)
		]
	test $ steps !! 4 == [
			(Vector3 2 (-8) 0, Vector3 (-3) (-2) 1),
			(Vector3 2 1 7, Vector3 2 1 1),
			(Vector3 2 3 (-6), Vector3 0 2 (-1)),
			(Vector3 2 (-9) 1, Vector3 1 (-1) (-1))
		]
	test $ steps !! 5 == [
			(Vector3 (-1) (-9) 2, Vector3 (-3) (-1) 2),
			(Vector3 4 1 5, Vector3 2 0 (-2)),
			(Vector3 2 2 (-4), Vector3 0 (-1) 2),
			(Vector3 3 (-7) (-1), Vector3 1 2 (-2))
		]
	test $ steps !! 6 == [
			(Vector3 (-1) (-7) 3, Vector3 0 2 1),
			(Vector3 3 0 0, Vector3 (-1) (-1) (-5)),
			(Vector3 3 (-2) 1, Vector3 1 (-4) 5),
			(Vector3 3 (-4) (-2), Vector3 0 3 (-1))
		]
	test $ steps !! 7 == [
			(Vector3 2 (-2) 1, Vector3 3 5 (-2)),
			(Vector3 1 (-4) (-4), Vector3 (-2) (-4) (-4)),
			(Vector3 3 (-7) 5, Vector3 0 (-5) 4),
			(Vector3 2 0 0, Vector3 (-1) 4 2)
		]
	test $ steps !! 8 == [
			(Vector3 5 2 (-2), Vector3 3 4 (-3)),
			(Vector3 2 (-7) (-5), Vector3 1 (-3) (-1)),
			(Vector3 0 (-9) 6, Vector3 (-3) (-2) 1),
			(Vector3 1 1 3, Vector3 (-1) 1 3)
		]
	test $ steps !! 9 == [
			(Vector3 5 3 (-4), Vector3 0 1 (-2)),
			(Vector3 2 (-9) (-3), Vector3 0 (-2) 2),
			(Vector3 0 (-8) 4, Vector3 0 1 (-2)),
			(Vector3 1 1 5, Vector3 0 0 2)
		]
	test $ steps !! 10 == [
			(Vector3 2 1 (-3), Vector3 (-3) (-2) 1),
			(Vector3 1 (-8) 0, Vector3 (-1) 1 3),
			(Vector3 3 (-6) 1, Vector3 3 2 (-3)),
			(Vector3 2 0 4, Vector3 1 (-1) (-1))
		]
	test $ energy (steps !! 10)== 179

	let init' = [
			(Vector3 (-8) (-10) 0, zero),
			(Vector3 5 5 10, zero),
			(Vector3 2 (-7) 3, zero),
			(Vector3 9 (-8) (-3), zero)
		]
	let steps' = iterate step init'
	test $ steps' !! 10 == [
			(Vector3 (-9) (-10) 1, Vector3 (-2) (-2) (-1)),
			(Vector3 4 10 9, Vector3 (-3) 7 (-2)),
			(Vector3 8 (-10) (-3), Vector3 5 (-1) (-2)),
			(Vector3 5 (-10) 3, Vector3 0 (-4) 5)
		]
	test $ steps' !! 20 == [
			(Vector3 (-10) 3 (-4), Vector3 (-5) 2 0),
			(Vector3 5 (-25) 6, Vector3 1 1 (-4)),
			(Vector3 13 1 1, Vector3 5 (-2) 2),
			(Vector3 0 1 7, Vector3 (-1) (-1) 2)
		]
	test $ steps' !! 30 == [
			(Vector3 15 (-6) (-9), Vector3 (-5) 4 0),
			(Vector3 (-4) (-11) 3, Vector3 (-3) (-10) 0),
			(Vector3 0 (-1) 11, Vector3 7 4 3),
			(Vector3 (-3) (-2) 5, Vector3 1 2 (-3))
		]
	test $ steps' !! 40 == [
			(Vector3 14 (-12) (-4), Vector3 11 3 0),
			(Vector3 (-1) 18 8, Vector3 (-5) 2 3),
			(Vector3 (-5) (-14) 8, Vector3 1 (-2) 0),
			(Vector3 0 (-12) (-2), Vector3 (-7) (-3) (-3))
		]
	test $ steps' !! 50 == [
			(Vector3 (-23) 4 1, Vector3 (-7) (-1) 2),
			(Vector3 20 (-31) 13, Vector3 5 3 4),
			(Vector3 (-4) 6 1, Vector3 (-1) 1 (-3)),
			(Vector3 15 1 (-5), Vector3 3 (-3) (-3))
		]
	test $ steps' !! 60 == [
			(Vector3 36 (-10) 6, Vector3 5 0 3),
			(Vector3 (-18) 10 9, Vector3 (-3) (-7) 5),
			(Vector3 8 (-12) (-3), Vector3 (-2) 1 (-7)),
			(Vector3 (-18) (-8) (-2), Vector3 0 6 (-1))
		]
	test $ steps' !! 70 == [
			(Vector3 (-33) (-6) 5, Vector3 (-5) (-4) 7),
			(Vector3 13 (-9) 2, Vector3 (-2) 11 3),
			(Vector3 11 (-8) 2, Vector3 8 (-6) (-7)),
			(Vector3 17 3 1, Vector3 (-1) (-1) (-3))
		]
	test $ steps' !! 80 == [
			(Vector3 30 (-8) 3, Vector3 3 3 0),
			(Vector3 (-2) (-4) 0, Vector3 4 (-13) 2),
			(Vector3 (-18) (-7) 15, Vector3 (-8) 2 (-2)),
			(Vector3 (-2) (-1) (-8), Vector3 1 8 0)
		]
	test $ steps' !! 90 == [
			(Vector3 (-25) (-1) 4, Vector3 1 (-3) 4),
			(Vector3 2 (-9) 0, Vector3 (-3) 13 (-1)),
			(Vector3 32 (-8) 14, Vector3 5 (-4) 6),
			(Vector3 (-1) (-2) (-8), Vector3 (-3) (-6) (-9))
		]
	test $ steps' !! 100 == [
			(Vector3 8 (-12) (-9), Vector3 (-7) 3 0),
			(Vector3 13 16 (-3), Vector3 3 (-11) (-5)),
			(Vector3 (-29) (-11) (-1), Vector3 (-3) 7 4),
			(Vector3 16 (-13) 23, Vector3 7 1 1)
		]
	test $ energy (steps' !! 100) == 1940

main :: IO ()
main = do
	let init = [
			(Vector3 5 13 (-3), zero),
			(Vector3 18 (-7) 13, zero),
			(Vector3 16 3 4, zero),
			(Vector3 0 8 8, zero)
		]
	let steps = iterate step init
	print $ energy $ steps !! 1000
