{-# OPTIONS_GHC -Wno-tabs #-}
import Control.Exception
import Vector

data Opcode = North | South | West | East | TurnLeft | TurnRight | Forward deriving (Eq, Show, Read)
type Operation = (Opcode, Integer)
type Vec = Vector2 Integer

north = Vector2 0 1
south = Vector2 0 (-1)
west = Vector2 (-1) 0
east = Vector2 1 0

getInput :: IO [Operation]
getInput = do
	dat <- readFile "12.txt"
	return $ map readOp $ lines dat

readOpcode :: Char -> Opcode
readOpcode 'N' = North
readOpcode 'S' = South
readOpcode 'W' = West
readOpcode 'E' = East
readOpcode 'L' = TurnLeft
readOpcode 'R' = TurnRight
readOpcode 'F' = Forward

readOp :: String -> Operation
readOp s = (readOpcode $ head s, read $ tail s)

rotL (Vector2 x y) = Vector2 (-y) x
rotR (Vector2 x y) = Vector2 y (-x)
rot2 (Vector2 x y) = Vector2 (-x) (-y)
rotate :: Operation -> Vec -> Vec
rotate (d,x) = go d (x `mod` 360)
	where
		go TurnLeft 0 = id
		go TurnLeft 90 = rotL
		go TurnLeft 180 = rot2
		go TurnLeft 270 = rotR
		go TurnRight 0 = id
		go TurnRight 90 = rotR
		go TurnRight 180 = rot2
		go TurnRight 270 = rotL

dist :: Vec -> Integer
dist (Vector2 x y) = abs x + abs y

initStateA = (Vector2 0 0, east)
doStepA :: Operation -> (Vec, Vec) -> (Vec, Vec)
doStepA (North, x) (pos, face) = (pos .+ north .* x, face)
doStepA (South, x) (pos, face) = (pos .+ south .* x, face)
doStepA (West, x) (pos, face) = (pos .+ west .* x, face)
doStepA (East, x) (pos, face) = (pos .+ east .* x, face)
doStepA op@(TurnLeft, _) (pos, face) = (pos, rotate op face)
doStepA op@(TurnRight, _) (pos, face) = (pos, rotate op face)
doStepA (Forward, x) (pos, face) = (pos .+ face .* x, face)
followA :: [Operation] -> Vec
followA = fst . foldl (flip doStepA) initStateA

initStateB = (Vector2 0 0, Vector2 10 1)
doStepB :: Operation -> (Vec, Vec) -> (Vec, Vec)
doStepB (North, x) (pos, wp) = (pos, wp .+ north .* x)
doStepB (South, x) (pos, wp) = (pos, wp .+ south .* x)
doStepB (West, x) (pos, wp) = (pos, wp .+ west .* x)
doStepB (East, x) (pos, wp) = (pos, wp .+ east .* x)
doStepB op@(TurnLeft, _) (pos, wp) = (pos, rotate op wp)
doStepB op@(TurnRight, _) (pos, wp) = (pos, rotate op wp)
doStepB (Forward, x) (pos, wp) = (pos .+ wp .* x, wp)
followB :: [Operation] -> Vec
followB = fst . foldl (flip doStepB) initStateB

tests = do
	check $ followA ops == Vector2 17 (-8)
	check $ followB ops == Vector2 214 (-72)
	where
		ops = map readOp $ lines "F10\nN3\nF7\nR90\nF11"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	ops <- getInput
	print $ dist $ followA ops
	print $ dist $ followB ops
