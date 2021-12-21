{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Control.Exception
import Utils

type GameState = ((Integer, Integer), (Integer, Integer), Bool)

getInput :: IO (Integer, Integer)
getInput = do
	dat <- readFile "21.txt"
	let [p1, p2] = map (read . drop 28) $ lines dat
	return (p1, p2)

incrementStep :: GameState -> Integer -> GameState
incrementStep ((p1,p2),(s1,s2),False) roll = ((p1', p2), (s1 + p1', s2), True)
	where p1' = (p1 + roll - 1) `mod` 10 + 1
incrementStep ((p1,p2),(s1,s2),True) roll = ((p1, p2'), (s1, s2 + p2'), False)
	where p2' = (p2 + roll - 1) `mod` 10 + 1

isWinning :: Integer -> GameState -> Bool
isWinning target (_,  (s1, s2), turn) = (if turn then s1 else s2) >= target
loserScore :: GameState -> Integer
loserScore (_, (s1, s2), turn) = if turn then s2 else s1

partA :: (Integer, Integer) -> Integer
partA inits = loserScore winstate * genericLength steps * 3
	where
		rolls = triples $ cycle [1..100]
		triples (a:b:c:rest) = (a+b+c):triples rest
		allsteps = scanl incrementStep (inits,(0,0),False) rolls
		(steps, (winstate:_)) = break (isWinning 1000) allsteps

diceRolls = M.assocs $ counter [ (a+b+c) | a<-[1..3], b<-[1..3], c<-[1..3] ]

partB :: (Integer, Integer) -> (Integer, Integer)
partB inits = worker (M.fromList [((inits,(0,0),False), 1)]) (0,0)
	where
		worker :: M.Map GameState Integer -> (Integer,Integer) -> (Integer, Integer)
		worker universes (w1, w2)
			| M.null universes = (w1, w2)
			| otherwise = worker universes' w'
				where
					totalScore (_, (s1, s2), _) = s1 + s2
					state@(_,_,turn) = minimumBy (compare `on` totalScore) $ M.keys universes
					count = universes M.! state
					newStates = [ (incrementStep state roll, count * rollcount) | (roll, rollcount) <- diceRolls ]
					winnerStates = sum $ map snd $ filter (isWinning 21.fst) newStates
					continueStates = filter (not.isWinning 21.fst) newStates
					w' = if turn then (w1, w2 + winnerStates) else (w1 + winnerStates, w2)
					universes' = foldl addCount (M.delete state universes) continueStates
					addCount m (k, v) = M.insert k (v + M.findWithDefault 0 k m) m

tests :: IO ()
tests = do
	check $ (partA sample) == 739785
	check $ (partB sample) == (444356092776315, 341960390180808)
	where
		sample = (4, 8)
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	--tests
	inits <- getInput
	print $ partA inits
	print $ uncurry max $ partB inits
