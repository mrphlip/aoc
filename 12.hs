{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Data.Set as S
import Utils

type PlanetCoord = (Integer, Integer) -- pos, vel

gravity :: Integer -> Integer -> Integer
gravity a b
	| a < b = 1
	| a > b = -1
	| a == b = 0

step :: [PlanetCoord] -> [PlanetCoord]
step planets = afterVelocity
	where
		grav = [ sum [ gravity p p' | (p',_) <- planets ] | (p,_) <- planets ]
		afterGravity = [ (p, v + g) | ((p, v), g) <- zip planets grav ]
		afterVelocity = [ (p + v, v) | (p, v) <- afterGravity ]

energy :: [PlanetCoord] -> [PlanetCoord] -> [PlanetCoord] -> Integer
energy xs ys zs = sum $ zipWith3 calcEnergy xs ys zs
	where
		calcEnergy (px, vx) (py, vy) (pz, vz) = calcPot px py pz * calcKin vx vy vz
		calcPot x y z = abs x + abs y + abs z
		calcKin x y z = abs x + abs y + abs z

findLoop :: [PlanetCoord] -> Integer
findLoop init = (+1) $ genericLength $ takeWhile (/=init) $ tail $ iterate step init

findLoop3 :: [PlanetCoord] -> [PlanetCoord] -> [PlanetCoord] -> Integer
findLoop3 xs ys zs = lcm (findLoop xs) $ lcm (findLoop ys) (findLoop zs)

tests :: IO ()
tests = do
	let init1x = [(-1,0), (2,0), (4,0), (3,0)]
	let init1y = [(0,0), (-10,0), (-8,0), (5,0)]
	let init1z = [(2,0), (-7,0), (8,0), (-1,0)]
	let steps1x = iterate step init1x
	let steps1y = iterate step init1y
	let steps1z = iterate step init1z
	test $ steps1x !! 1 == [(2, 3), (3, 1), (1, -3), (2, -1)]
	test $ steps1y !! 1 == [(-1, -1), (-7, 3), (-7, 1), (2, -3)]
	test $ steps1z !! 1 == [(1, -1), (-4, 3), (5, -3), (0, 1)]
	test $ steps1x !! 2 == [(5, 3), (1, -2), (1, 0), (1, -1)]
	test $ steps1y !! 2 == [(-3, -2), (-2, 5), (-4, 3), (-4, -6)]
	test $ steps1z !! 2 == [(-1, -2), (2, 6), (-1, -6), (2, 2)]
	test $ steps1x !! 3 == [(5, 0), (0, -1), (2, 1), (1, 0)]
	test $ steps1y !! 3 == [(-6, -3), (0, 2), (1, 5), (-8, -4)]
	test $ steps1z !! 3 == [(-1, 0), (6, 4), (-5, -4), (2, 0)]
	test $ steps1x !! 4 == [(2, -3), (2, 2), (2, 0), (2, 1)]
	test $ steps1y !! 4 == [(-8, -2), (1, 1), (3, 2), (-9, -1)]
	test $ steps1z !! 4 == [(0, 1), (7, 1), (-6, -1), (1, -1)]
	test $ steps1x !! 5 == [(-1, -3), (4, 2), (2, 0), (3, 1)]
	test $ steps1y !! 5 == [(-9, -1), (1, 0), (2, -1), (-7, 2)]
	test $ steps1z !! 5 == [(2, 2), (5, -2), (-4, 2), (-1, -2)]
	test $ steps1x !! 6 == [(-1, 0), (3, -1), (3, 1), (3, 0)]
	test $ steps1y !! 6 == [(-7, 2), (0, -1), (-2, -4), (-4, 3)]
	test $ steps1z !! 6 == [(3, 1), (0, -5), (1, 5), (-2, -1)]
	test $ steps1x !! 7 == [(2, 3), (1, -2), (3, 0), (2, -1)]
	test $ steps1y !! 7 == [(-2, 5), (-4, -4), (-7, -5), (0, 4)]
	test $ steps1z !! 7 == [(1, -2), (-4, -4), (5, 4), (0, 2)]
	test $ steps1x !! 8 == [(5, 3), (2, 1), (0, -3), (1, -1)]
	test $ steps1y !! 8 == [(2, 4), (-7, -3), (-9, -2), (1, 1)]
	test $ steps1z !! 8 == [(-2, -3), (-5, -1), (6, 1), (3, 3)]
	test $ steps1x !! 9 == [(5, 0), (2, 0), (0, 0), (1, 0)]
	test $ steps1y !! 9 == [(3, 1), (-9, -2), (-8, 1), (1, 0)]
	test $ steps1z !! 9 == [(-4, -2), (-3, 2), (4, -2), (5, 2)]
	test $ steps1x !! 10 == [(2, -3), (1, -1), (3, 3), (2, 1)]
	test $ steps1y !! 10 == [(1, -2), (-8, 1), (-6, 2), (0, -1)]
	test $ steps1z !! 10 == [(-3, 1), (0, 3), (1, -3), (4, -1)]
	test $ energy (steps1x !! 10) (steps1y !! 10) (steps1z !! 10) == 179
	test $ findLoop3 init1x init1y init1z == 2772

	let init2x = [(-8, 0), (5, 0), (2, 0), (9, 0)]
	let init2y = [(-10, 0), (5, 0), (-7, 0), (-8, 0)]
	let init2z = [(0, 0), (10, 0), (3, 0), (-3, 0)]
	let steps2x = iterate step init2x
	let steps2y = iterate step init2y
	let steps2z = iterate step init2z
	test $ steps2x !! 10 == [(-9, -2), (4, -3), (8, 5), (5, 0)]
	test $ steps2y !! 10 == [(-10, -2), (10, 7), (-10, -1), (-10, -4)]
	test $ steps2z !! 10 == [(1, -1), (9, -2), (-3, -2), (3, 5)]
	test $ steps2x !! 20 == [(-10, -5), (5, 1), (13, 5), (0, -1)]
	test $ steps2y !! 20 == [(3, 2), (-25, 1), (1, -2), (1, -1)]
	test $ steps2z !! 20 == [(-4, 0), (6, -4), (1, 2), (7, 2)]
	test $ steps2x !! 30 == [(15, -5), (-4, -3), (0, 7), (-3, 1)]
	test $ steps2y !! 30 == [(-6, 4), (-11, -10), (-1, 4), (-2, 2)]
	test $ steps2z !! 30 == [(-9, 0), (3, 0), (11, 3), (5, -3)]
	test $ steps2x !! 40 == [(14, 11), (-1, -5), (-5, 1), (0, -7)]
	test $ steps2y !! 40 == [(-12, 3), (18, 2), (-14, -2), (-12, -3)]
	test $ steps2z !! 40 == [(-4, 0), (8, 3), (8, 0), (-2, -3)]
	test $ steps2x !! 50 == [(-23, -7), (20, 5), (-4, -1), (15, 3)]
	test $ steps2y !! 50 == [(4, -1), (-31, 3), (6, 1), (1, -3)]
	test $ steps2z !! 50 == [(1, 2), (13, 4), (1, -3), (-5, -3)]
	test $ steps2x !! 60 == [(36, 5), (-18, -3), (8, -2), (-18, 0)]
	test $ steps2y !! 60 == [(-10, 0), (10, -7), (-12, 1), (-8, 6)]
	test $ steps2z !! 60 == [(6, 3), (9, 5), (-3, -7), (-2, -1)]
	test $ steps2x !! 70 == [(-33, -5), (13, -2), (11, 8), (17, -1)]
	test $ steps2y !! 70 == [(-6, -4), (-9, 11), (-8, -6), (3, -1)]
	test $ steps2z !! 70 == [(5, 7), (2, 3), (2, -7), (1, -3)]
	test $ steps2x !! 80 == [(30, 3), (-2, 4), (-18, -8), (-2, 1)]
	test $ steps2y !! 80 == [(-8, 3), (-4, -13), (-7, 2), (-1, 8)]
	test $ steps2z !! 80 == [(3, 0), (0, 2), (15, -2), (-8, 0)]
	test $ steps2x !! 90 == [(-25, 1), (2, -3), (32, 5), (-1, -3)]
	test $ steps2y !! 90 == [(-1, -3), (-9, 13), (-8, -4), (-2, -6)]
	test $ steps2z !! 90 == [(4, 4), (0, -1), (14, 6), (-8, -9)]
	test $ steps2x !! 100 == [(8, -7), (13, 3), (-29, -3), (16, 7)]
	test $ steps2y !! 100 == [(-12, 3), (16, -11), (-11, 7), (-13, 1)]
	test $ steps2z !! 100 == [(-9, 0), (-3, -5), (-1, 4), (23, 1)]
	test $ energy (steps2x !! 100) (steps2y !! 100) (steps2z !! 100) == 1940
	test $ findLoop3 init2x init2y init2z == 4686774924

main :: IO ()
main = do
	let initx = [(5,0), (18,0), (16,0), (0,0)]
	let inity = [(13,0), (-7,0), (3,0), (8,0)]
	let initz = [(-3,0), (13,0), (4,0), (8,0)]
	let stepsx = iterate step initx
	let stepsy = iterate step inity
	let stepsz = iterate step initz
	print $ energy (stepsx !! 1000) (stepsy !! 1000) (stepsz !! 1000)
	print $ findLoop3 initx inity initz
