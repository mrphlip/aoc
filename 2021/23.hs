{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import Control.Exception
import Control.Monad
import Dijkstra

type State = [[Maybe Int]]

makeInitState :: [String] -> State
makeInitState rows = (transpose $ map (map fromLetter) rows) ++ [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]
	where fromLetter c = Just $ ord c - ord 'A'

showState :: State -> String
showState state = unlines $ [toprow, hallway] ++ [home i | i <- [0..length (head state) - 1]] ++ [bottomrow]
	where
		toprow = "#############"
		hallway = ['#', c 4 0, c 4 1, ' ', c 4 2, ' ', c 4 3, ' ', c 4 4, ' ', c 4 5, c 4 6, '#']
		home n = [b, b, '#', c 0 n, '#', c 1 n, '#', c 2 n, '#', c 3 n, '#', b, b]
			where b = if n == 0 then '#' else ' '
		bottomrow = "  #########  "
		c n m = case state !! n !! m of Nothing -> ' '; (Just x) -> chr (x + ord 'A')

hallwaypos :: Int -> Int
hallwaypos 0 = 0
hallwaypos 1 = 1
hallwaypos 2 = 3
hallwaypos 3 = 5
hallwaypos 4 = 7
hallwaypos 5 = 9
hallwaypos 6 = 10

calcdist :: Int -> Int -> Int -> Int -> Integer
calcdist 4 fromb 4 tob = toInteger $ abs (hallwaypos fromb - hallwaypos tob)
calcdist froma fromb 4 tob = toInteger $ fromb+1 + abs (2*(froma+1) - hallwaypos tob)
calcdist 4 fromb toa tob = toInteger $ abs (hallwaypos fromb - 2*(toa+1)) + tob+1
calcdist froma fromb toa tob = toInteger $ fromb+1 + abs (2*(froma+1) - 2*(toa+1)) + tob+1

setval :: Int -> a -> [a] -> [a]
setval _ _ [] = []
setval 0 x (_:rest) = x:rest
setval n x (a:rest) = a:setval (n-1) x rest

setval2 :: Int -> Int -> a -> [[a]] -> [[a]]
setval2 _ _ _ [] = []
setval2 0 m x (a:rest) = setval m x a:rest
setval2 n m x (a:rest) = a:setval2 (n-1) m x rest

nextState :: Int -> Int -> Int -> Int -> State -> (State, (), Integer)
nextState froma fromb toa tob state = (setval2 froma fromb Nothing $ setval2 toa tob (Just target) state, (), calcdist froma fromb toa tob * 10^target)
	where (Just target) = state !! froma !! fromb

neighbours :: State -> [(State, (), Integer)]
neighbours state = join $ [moveOut state i | i <- [0..3]] ++ [moveHome state i | i <- [0..6]]

moveOut :: State -> Int -> [(State, (), Integer)]
moveOut state froma = if homeReady then [] else map makeState $ targetleft ++ targetright
	where
		home = state !! froma
		hallway = state !! 4
		homeReady = all (\x -> isNothing x || x == (Just froma)) home
		fromb = length $ takeWhile isNothing home
		targetleft = takeWhile (isNothing . (hallway !!)) [froma+1, froma .. 0]
		targetright = takeWhile (isNothing . (hallway !!)) [froma+2 .. 6]
		makeState tob = nextState froma fromb 4 tob state

moveHome :: State -> Int -> [(State, (), Integer)]
moveHome state fromb = if cellEmpty then [] else if not hallwayReady || not homeReady then [] else [nextState 4 fromb toa tob state]
	where
		hallway = state !! 4
		cellEmpty = isNothing $ hallway !! fromb
		(Just toa) = hallway !! fromb
		home = state !! toa
		hallwayThrough = if toa+1 > fromb then [fromb+1..toa+1] else [toa+2..fromb-1]
		hallwayReady = all isNothing $ map (hallway !!) hallwayThrough
		homeReady = all (\x -> isNothing x || x == (Just toa)) home
		tob = subtract 1 $ length $ takeWhile isNothing home

heuristic :: State -> Integer
heuristic state = fromHallway + fromWrongHome + furtherDepth
	where
		fromHallway = sum $ map fromHallwayCell [0..6]
		fromHallwayCell n = case state !! 4 !! n of Nothing -> 0; (Just x) -> calcdist 4 n x 0 * 10^x
		depth = length $ head state
		notHome n = depth - (length $ takeWhile (==(Just n)) $ reverse (state !! n))
		notHomeCount = map notHome [0..3]
		fromWrongHome = sum [ fromWrongHomeCell a b | a <- [0..3], b <- [0..(notHomeCount !! a) - 1]]
		fromWrongHomeCell a b = case state !! a !! b of Nothing -> 0; (Just x) -> calcdist a b x 0 * 10^x
		furtherDepthCol n = let d = notHomeCount !! n in toInteger $ d * (d - 1) `div` 2 * 10^n
		furtherDepth = sum $ map furtherDepthCol [0..3]

isFinalState :: State -> Bool
isFinalState state = all (\(r,n) -> all (==(Just n)) r) $ zip state [0..3]

solveState :: State -> Integer
solveState state = let (n, _, _) = distmap M.! finalstate in n
	where (distmap, Just finalstate) = buildDistMapAstar state neighbours isFinalState heuristic

solvePartA a b = solveState $ makeInitState [a, b]
solvePartB a b = solveState $ makeInitState [a, "DCBA", "DBAC", b]

tests :: IO ()
tests = do
	check $ (solvePartA "BCBD" "ADCA") == 12521
	check $ (solvePartB "BCBD" "ADCA") == 44169
	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	--tests
	print $ solvePartA "DACA" "DCBB"
	print $ solvePartB "DACA" "DCBB"
