{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Map.Strict as M
import Data.Set as S
import Data.List
import Data.Maybe
import Control.Monad
import Control.Exception

root = "COM"

readInput :: IO [(String, String)]
readInput = do
	dat <- readFile "06.txt"
	return $ parseInput dat

parseInput :: String -> [(String, String)]
parseInput dat = do
		line <- lines dat
		guard $ not $ Prelude.null line
		let (a, b) = break (==')') line
		return (tail b, a)

type Node = (String, Integer) -- parent, depth
makeTree :: [(String,String)] -> Map String Node
makeTree orbits = orbittree
	where
		parenttree = M.fromList orbits :: Map String String
		planets = S.insert root (M.keysSet parenttree)
		makenode planet
			| planet == root = ("", 0)
			| otherwise = let parent = parenttree ! planet in (parent, snd (orbittree ! parent) + 1)
		orbittree = M.fromSet makenode planets

countOrbits tree = sum $ Prelude.map snd $ M.elems tree

ancestors :: Map String Node -> String -> [String]
ancestors tree node
	| node == root = [root]
	| otherwise = node : ancestors tree (fst $ tree ! node)

commonParent :: Map String Node -> String -> String -> String
commonParent tree a b = fromMaybe root $ find (`S.member` target) $ ancestors tree b
	where target = S.fromList $ ancestors tree a 

transferDist tree a b = depth a + depth b - 2 * depth parent
	where
		parent = commonParent tree a b
		depth x = snd $ tree ! x

tests = do
	let sample = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\n"
	let orbits = parseInput sample
	let orbittree = makeTree orbits
	check $ orbittree == M.fromList [
			("COM", ("", 0)),
			("B", ("COM", 1)),
			("C", ("B", 2)),
			("D", ("C", 3)),
			("E", ("D", 4)),
			("F", ("E", 5)),
			("G", ("B", 2)),
			("H", ("G", 3)),
			("I", ("D", 4)),
			("J", ("E", 5)),
			("K", ("J", 6)),
			("L", ("K", 7))
		]
	check $ countOrbits orbittree == 42

	let sample2 = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN\n"
	let orbits2 = parseInput sample2
	let orbittree2 = makeTree orbits2
	check $ ancestors orbittree2 "K" == ["K", "J", "E", "D", "C", "B", "COM"]
	check $ commonParent orbittree2 "YOU" "SAN" == "D"
	check $ transferDist orbittree2 "YOU" "SAN" == 6
	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"


main = do
	orbits <- readInput
	let orbittree = makeTree orbits
	print $ countOrbits orbittree
	print $ transferDist orbittree "YOU" "SAN" - 2
