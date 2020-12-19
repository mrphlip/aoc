{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Text.ParserCombinators.ReadP as P
import Text.Regex.Posix
import Control.Exception
import Utils

data Production = NonTerminal Integer | Terminal String deriving (Eq, Show, Read)
type Rule = (Integer, [[Production]])

getInput :: IO ([Rule], [String])
getInput = do
	dat <- readFile "19.txt"
	return $ readInput dat

readInput :: String -> ([Rule], [String])
readInput dat = (rules, messages)
	where
		[rulestr, messagestr] = splitOn "\n\n" dat
		rules = map readRule $ lines rulestr
		messages = lines messagestr

readRule :: String -> Rule
readRule = runReadP readLine
	where
		readLine = do
			key <- readInt
			P.skipSpaces
			P.char ':'
			P.skipSpaces
			productions <- P.sepBy1 readProductions $ P.skipSpaces >> P.char '|' >> P.skipSpaces
			return (key, productions)
		readProductions = P.sepBy1 readProduction P.skipSpaces
		readProduction = (NonTerminal <$> readInt) P.+++ (Terminal <$> readString)
		readString = P.readS_to_P reads :: P.ReadP String
		readInt = P.readS_to_P reads :: P.ReadP Integer

buildRegexA :: [Rule] -> Regex
buildRegexA rules = makeRegex $ (\x -> "^" ++ x ++ "$") $ getbuilt 0
	where
		rulemap = M.fromList rules
		builtmemo = map build [0..]
		getbuilt = genericIndex builtmemo
		build n = (\x -> "(" ++ x ++ ")") $ intercalate "|" $ map (concat.map buildprod) $ rulemap M.! n
		buildprod (NonTerminal n) = getbuilt n
		buildprod (Terminal s) = s

buildRegexB :: [Rule] -> Regex
buildRegexB rules = makeRegex $ (\x -> "^" ++ x ++ "$") $ getbuilt 0
	where
		rulemap = M.fromList rules
		builtmemo = map build [0..]
		getbuilt = genericIndex builtmemo
		build 8 = "(" ++ getbuilt 42 ++ "+)"
		build 11 = "(" ++ intercalate "|" (map reps [1..10]) ++ ")"
			where
				a = getbuilt 42
				al = length a
				b = getbuilt 31
				bl = length b
				reps n = take (al * n) (cycle a) ++ take (bl * n) (cycle b)
		build n = (\x -> "(" ++ x ++ ")") $ intercalate "|" $ map (concat.map buildprod) $ rulemap M.! n
		buildprod (NonTerminal n) = getbuilt n
		buildprod (Terminal s) = s

tests :: IO ()
tests = do
	check $ map (matchTest regexA) messagesA == [True, False, True, False, False]
	check $ (length $ filter (matchTest regexBA) messagesB) == 3
	check $ (length $ filter (matchTest regexBB) messagesB) == 12
	where
		(rulesA, messagesA) = readInput "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb"
		(rulesB, messagesB) = readInput "42: 9 14 | 10 1\n9: 14 27 | 1 26\n10: 23 14 | 28 1\n1: \"a\"\n11: 42 31\n5: 1 14 | 15 1\n19: 14 1 | 14 14\n12: 24 14 | 19 1\n16: 15 1 | 14 14\n31: 14 17 | 1 13\n6: 14 14 | 1 14\n2: 1 24 | 14 4\n0: 8 11\n13: 14 3 | 1 12\n15: 1 | 14\n17: 14 2 | 1 7\n23: 25 1 | 22 14\n28: 16 1\n4: 1 1\n20: 14 14 | 1 15\n3: 5 14 | 16 1\n27: 1 6 | 14 18\n14: \"b\"\n21: 14 1 | 1 14\n25: 1 1 | 1 14\n22: 14 14\n8: 42\n26: 14 22 | 1 20\n18: 15 15\n7: 14 5 | 1 21\n24: 14 1\n\nabbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\nbbabbbbaabaabba\nbabbbbaabbbbbabbbbbbaabaaabaaa\naaabbbbbbaaaabaababaabababbabaaabbababababaaa\nbbbbbbbaaaabbbbaaabbabaaa\nbbbababbbbaaaaaaaabbababaaababaabab\nababaaaaaabaaab\nababaaaaabbbaba\nbaabbaaaabbaaaababbaababb\nabbbbabbbbaaaababbbbbbaaaababb\naaaaabbaabaaaaababaa\naaaabbaaaabbaaa\naaaabbaabbaaaaaaabbbabbbaaabbaabaaa\nbabaaabbbaaabaababbaabababaaab\naabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
		regexA = buildRegexA rulesA
		regexBA = buildRegexA rulesB
		regexBB = buildRegexB rulesB
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	(rules, messages) <- getInput
	let regexA = buildRegexA rules
	print $ length $ filter (matchTest regexA) messages
	let regexB = buildRegexB rules
	print $ length $ filter (matchTest regexB) messages
