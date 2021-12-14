{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import qualified Data.Map.Strict as M
import Control.Exception
import Control.Monad
import Utils

type Productions = M.Map (Char,Char) Char
type StateCount = M.Map (Char,Char) Integer

getInput :: IO (String, Productions)
getInput = do
	dat <- readFile "14.txt"
	return $ parseInput dat
parseInput :: String -> (String, Productions)
parseInput dat = (starter, M.fromList $ map parseProduction productions)
	where (starter:"":productions) = lines dat
parseProduction :: String -> ((Char, Char), Char)
parseProduction [a,b,' ','-','>',' ',c] = ((a,b),c)

makeCounts :: String -> StateCount
makeCounts s = counter $ zip s (tail s ++ "$")

doStep :: Productions -> StateCount -> StateCount
doStep productions state = counterAccum $ join $ map expand $ M.assocs state
	where expand ((a,b),n) = case productions M.!? (a,b) of
		(Just c) -> [((a,c),n), ((c,b),n)]
		Nothing -> [((a,b),n)]

countLetters :: StateCount -> M.Map Char Integer
countLetters state = counterAccum $ map (\((c,_),n) -> (c,n)) $ M.assocs state

countRange :: M.Map Char Integer -> Integer
countRange counts = maximum vals - minimum vals
	where vals = M.elems counts

doCalc :: Integer -> Productions -> String -> Integer
doCalc n productions starter = countRange $ countLetters $ head $ genericDrop n $ iterate (doStep productions) $ makeCounts starter

tests :: IO ()
tests = do
	check $ (doCalc 10 productions starter) == 1588
	check $ (doCalc 40 productions starter) == 2188189693529
	where
		(starter, productions) = parseInput "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	(starter, productions) <- getInput
	print $ doCalc 10 productions starter
	print $ doCalc 40 productions starter
