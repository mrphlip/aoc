{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Maybe
import qualified Text.ParserCombinators.ReadP as P
import Control.Exception
import Control.Monad
import GHC.Base ((<|>))
import Utils
import Debug.Trace

data Node = Pair Node Node | Regular Integer deriving (Eq)

getInput :: IO [Node]
getInput = do
	dat <- readFile "18.txt"
	return $ map read $ lines dat

instance Read Node where
	readsPrec _ = P.readP_to_S $ P.skipSpaces >> parseNode
		where
			parseInt = P.readS_to_P reads :: P.ReadP Integer
			parseRegular = Regular <$> parseInt
			parsePair = do
				P.char '['
				P.skipSpaces
				a <- parseNode
				P.skipSpaces
				P.char ','
				P.skipSpaces
				b <- parseNode
				P.skipSpaces
				P.char ']'
				P.skipSpaces
				return $ Pair a b
			parseNode = parseRegular P.+++ parsePair

instance Show Node where
	showsPrec _ (Regular n) = shows n
	showsPrec _ (Pair a b) = ('[':) . shows a . (',':) . shows b . (']':)

getAddr :: [Bool] -> Node -> Node
getAddr _ node@(Regular _) = node
getAddr [] node = node
getAddr (False:rest) (Pair a b) = getAddr rest a
getAddr (True:rest) (Pair a b) = getAddr rest b

setAddr :: [Bool] -> Node -> Node -> Node
setAddr _ replacement (Regular _) = replacement
setAddr [] replacement _ = replacement
setAddr (False:rest) replacement (Pair a b) = Pair (setAddr rest replacement a) b
setAddr (True:rest) replacement (Pair a b) = Pair a (setAddr rest replacement b)

tryExplode :: Node -> Maybe Node
tryExplode root = if isJust maybeNested then Just patchedroot else Nothing
	where
		findNested :: Node -> [Bool] -> Integer -> Maybe ([Bool], Node)
		findNested (Regular _) _ _ = Nothing
		findNested n@(Pair a b) revaddr depth
			|	depth >= 4 = Just (revaddr, n)
			| otherwise = leftside <|> rightside
			where
				leftside = findNested a (False:revaddr) (depth+1)
				rightside = findNested b (True:revaddr) (depth+1)
		maybeNested = findNested root [] 0
		(Just (revaddr, (Pair (Regular badleft) (Regular badright)))) = maybeNested
		addr = reverse revaddr
		predecessor [] = Nothing
		predecessor (False:rest) = predecessor rest
		predecessor (True:rest) = Just $ reverse rest ++ [False] ++ repeat True
		successor [] = Nothing
		successor (False:rest) = Just $ reverse rest ++ [True] ++ repeat False
		successor (True:rest) = successor rest
		maybeAdd Nothing _ node = node
		maybeAdd (Just addr) n node = setAddr addr (Regular (n + val)) node
			where (Regular val) = getAddr addr node

		patchedroot = setAddr addr (Regular 0) $ maybeAdd (predecessor revaddr) badleft $ maybeAdd (successor revaddr) badright $ root

trySplit :: Node -> Maybe Node
trySplit root = if isJust maybeLarge then Just patchedroot else Nothing
	where
		findLarge :: Node -> [Bool] -> Maybe ([Bool], Integer)
		findLarge (Regular n) revaddr
			| n >= 10 = Just (revaddr, n)
			| otherwise = Nothing
		findLarge (Pair a b) revaddr = leftside <|> rightside
			where
				leftside = findLarge a (False:revaddr)
				rightside = findLarge b (True:revaddr)
		maybeLarge = findLarge root []
		(Just (revaddr, value)) = maybeLarge
		addr = reverse revaddr
		replacement = Pair (Regular (value `div` 2)) (Regular ((value + 1) `div` 2))
		patchedroot = setAddr addr replacement root

tryReduce :: Node -> Maybe Node
tryReduce node = tryExplode node <|> trySplit node

runReduce :: Node -> Node
runReduce node = case tryReduce node of
	Just node' -> runReduce node'
	Nothing -> node

addNode :: Node -> Node -> Node
addNode a b = runReduce $ Pair a b

sumNodes = foldl1 addNode

magnitude :: Node -> Integer
magnitude (Regular n) = n
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

highestPair :: [Node] -> Integer
highestPair nodes = maximum [ magnitude $ a `addNode` b | a<-nodes, b<-nodes ]

tests :: IO ()
tests = do
	check $ (read "[[[[4,3],4],4],[7,[[8,4],9]]]" `addNode` read "[1,1]") == read "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
	check $ (sumNodes sample) == read "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
	check $ (sumNodes sample2) == read "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"
	check $ (magnitude $ sumNodes sample2) == 4140
	check $ (highestPair sample2) == 3993
	where
		sample = map read $ lines "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]\n[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]\n[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]\n[7,[5,[[3,8],[1,4]]]]\n[[2,[2,2]],[8,[8,1]]]\n[2,9]\n[1,[[[9,3],9],[[9,0],[0,7]]]]\n[[[5,[7,4]],7],1]\n[[[[4,2],2],6],[8,7]]"
		sample2 = map read $ lines "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	nodes <- getInput
	print $ magnitude $ sumNodes nodes
	print $ highestPair nodes
