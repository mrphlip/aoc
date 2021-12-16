{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Char
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Exception
import Utils

data Node = Single String | Multi String deriving (Eq, Ord, Show, Read)

getInput :: IO [(Node, Node)]
getInput = do
	dat <- readFile "12.txt"
	return $ map parseInputLine $ lines dat
parseInputLine :: String -> (Node, Node)
parseInputLine line = let [a,b] = split '-' line in (makeNode a, makeNode b)
makeNode :: String -> Node
makeNode n
	| isUpper $ head n = Multi n
	| otherwise = Single n

countPaths :: Bool -> [(Node, Node)] -> Integer
countPaths allowDup edgelist = countPathsFrom allowDup S.empty (Single "start")
	where
		nodes :: [Node]
		nodes = nub $ map fst edgelist ++ map snd edgelist
		edges :: M.Map Node [Node]
		edges = M.fromListWith (++) $ [(a,[b]) | (a,b) <- edgelist] ++ [(b,[a]) | (a,b) <- edgelist]
		countPathsFrom :: Bool -> S.Set Node -> Node -> Integer
		countPathsFrom _ _ (Single "end") = 1
		countPathsFrom allowDup seen n
			|	n `S.member` seen && doAllowDup = countPathsFrom False (S.delete n seen) n
			|	n `S.member` seen = 0
			| otherwise = sum $ map (countPathsFrom allowDup seen') $ edges M.! n
			where
				seen' = addToSeen seen n
				doAllowDup = allowDup && n /= (Single "start") && n /= (Single "end")
		addToSeen :: S.Set Node -> Node -> S.Set Node
		addToSeen seen n@(Single _) = S.insert n seen
		addToSeen seen (Multi _) = seen

tests :: IO ()
tests = do
	check $ (countPaths False sample1) == 10
	check $ (countPaths False sample2) == 19
	check $ (countPaths False sample3) == 226
	check $ (countPaths True sample1) == 36
	check $ (countPaths True sample2) == 103
	check $ (countPaths True sample3) == 3509
	where
		sample1 = map parseInputLine ["start-A", "start-b", "A-c", "A-b", "b-d", "A-end", "b-end"]
		sample2 = map parseInputLine ["dc-end","HN-start","start-kj","dc-start","dc-HN","LN-dc","HN-end","kj-sa","kj-HN","kj-dc"]
		sample3 = map parseInputLine ["fs-end","he-DX","fs-he","start-DX","pj-DX","end-zg","zg-sl","zg-pj","pj-he","RW-he","fs-DX","pj-RW","zg-RW","start-pj","he-WI","zg-he","pj-fs","start-RW"]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	print $ countPaths False dat
	print $ countPaths True dat
