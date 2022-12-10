{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Control.Exception
import Direction

type Step = (Integer, Direction)

getInput :: IO [Step]
getInput = map parseStep <$> lines <$> readFile "09.txt"

parseStep :: String -> Step
parseStep s = let [d, n] = words s in (read n, readDir d)
readDir "U" = UpDir
readDir "D" = DownDir
readDir "L" = LeftDir
readDir "R" = RightDir

flattenSteps :: [Step] -> [Direction]
flattenSteps = concat . map (uncurry genericReplicate)

followTail :: Integer -> [Step] -> [(Integer, Integer)]
followTail len steps = map last $ scanl nextState initState $ flattenSteps steps
	where
		initState = genericReplicate len (0, 0)
		nextState (h:rest) d = scanl tailStep (step d h) rest

tailStep :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
tailStep (hx, hy) (tx, ty)
	| adx <= 1 && ady <= 1 = (tx, ty)
	| otherwise = (tx + sdx, ty + sdy)
	where
		dx = hx - tx
		dy = hy - ty
		adx = abs dx
		ady = abs dy
		sdx = signum dx
		sdy = signum dy

tailCount :: Integer -> [Step] -> Integer
tailCount len steps = genericLength $ nub $ followTail len steps

tests :: IO ()
tests = do
	check $ tailCount 2 testData == 13
	check $ tailCount 10 testData2 == 36
	where
		testData = map parseStep ["R 4","U 4","L 3","D 1","R 4","D 1","L 5","R 2"]
		testData2 = map parseStep ["R 5","U 8","L 8","D 3","R 17","D 10","L 25","U 20"]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	print $ tailCount 2 dat
	print $ tailCount 10 dat
