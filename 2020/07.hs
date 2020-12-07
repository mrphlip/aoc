{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Char
import Data.List
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Exception

data Rule = Rule String [(String, Integer)] deriving (Eq, Show, Read)

getInput :: IO [Rule]
getInput = do
	dat <- readFile "07.txt"
	return $ map parseInputLine $ lines dat

parseInputLine :: String -> Rule
parseInputLine line = fst $ head $ filter (null.snd) $ P.readP_to_S readLine line
	where
		readLine :: P.ReadP Rule
		readLine = do
			container <- readBag
			P.skipSpaces
			P.string "contain"
			P.optional $ P.char 's'
			P.skipSpaces
			items <- readNoItems P.+++ readItems
			P.skipSpaces
			P.optional $ P.char '.'
			return $ Rule container items
		readInt = P.readS_to_P reads :: P.ReadP Integer
		readBag = do
			(bag, _) <- P.gather $ do
				P.munch1 (not . isSpace)
				P.skipSpaces
				P.munch1 (not . isSpace)
			P.skipSpaces
			P.string "bag"
			P.optional $ P.char 's'
			return bag
		readItem :: P.ReadP (String, Integer)
		readItem = do
			P.skipSpaces
			count <- readInt
			P.skipSpaces
			bag <- readBag
			return (bag, count)
		readItems :: P.ReadP [(String, Integer)]
		readItems = P.sepBy1 readItem $ P.skipSpaces >> P.char ','
		readNoItems :: P.ReadP [(String, Integer)]
		readNoItems = do
			P.string "no other bags"
			return []

buildInvDepMap :: [Rule] -> M.Map String (S.Set String)
buildInvDepMap rules = M.fromListWith S.union [(content, S.singleton container) | Rule container contents <- rules, (content, _) <- contents]

traceInvDepMap :: M.Map String (S.Set String) -> String -> [String]
traceInvDepMap depmap root = worker [root] S.empty
	where
		worker [] seen = S.toList seen
		worker (node:rest) seen
			| node `S.member` seen = worker rest seen
			| otherwise = worker (rest ++ S.toList (M.findWithDefault S.empty node depmap)) (S.insert node seen)

traceDepMap :: [Rule] -> String -> [(String, Integer)]
traceDepMap rules root = M.toList $ worker root
	where
		depmap :: M.Map String [(String, Integer)]
		depmap = M.fromList [(container, contents) | Rule container contents <- rules]
		worker :: String -> M.Map String Integer
		worker node = foldl (M.unionWith (+)) (M.singleton node 1) nested
			where
				nested :: [M.Map String Integer]
				nested = [M.map (*count) $ worker content | (content, count) <- depmap M.! node]

tests = do
	check $ length (traceInvDepMap invdepmap "shiny gold") == 5
	check $ (sum $ map snd $ traceDepMap rules "shiny gold") == 33
	check $ (sum $ map snd $ traceDepMap rules2 "shiny gold") == 127
	where
		rules = map parseInputLine $ lines "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags."
		invdepmap = buildInvDepMap rules
		rules2 = map parseInputLine $ lines "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags."
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	rules <- getInput
	let invdepmap = buildInvDepMap rules
	-- subtract 1 is to remove "shiny gold" itself from the options
	print $ subtract 1 $ length $ traceInvDepMap invdepmap "shiny gold"
	print $ subtract 1 $ sum $ map snd $ traceDepMap rules "shiny gold"
