{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Char
import Data.List
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Exception
import Utils

getInput :: IO [([String], [String])]
getInput = do
	dat <- readFile "21.txt"
	return $ map parseLine $ lines dat

parseLine :: String -> ([String], [String])
parseLine = runReadP readLine
	where
		readLine = do
			ingredients <- P.sepBy1 readWord P.skipSpaces
			P.skipSpaces
			P.char '('
			P.skipSpaces
			P.string "contains"
			P.skipSpaces
			allergens <- P.sepBy1 readWord $ P.skipSpaces >> P.char ',' >> P.skipSpaces
			P.skipSpaces
			P.char ')'
			return (ingredients, allergens)
		readWord = P.munch1 isLetter

processAllergens :: [([String], [String])] -> M.Map String (S.Set String)
processAllergens dat = M.fromListWith S.intersection [(a, S.fromList is) | (is, as) <- dat, a <- as]

allIngredients :: [([String], [String])] -> S.Set String
allIngredients dat = S.fromList [i | (is, _) <- dat, i <- is]

cleanIngredients :: S.Set String -> M.Map String (S.Set String) -> S.Set String
cleanIngredients is amap = foldl S.difference is $ M.elems amap

calcPartA :: [([String], [String])] -> Integer
calcPartA dat = genericLength [i | (is, _) <- dat, i <- is, i `S.member` clean]
	where
		amap = processAllergens dat
		is = allIngredients dat
		clean = cleanIngredients is amap

solvePuzzle :: [([String], [String])] -> M.Map String String
solvePuzzle dat = M.map (head . S.elems) $ doSolve amap $ M.keysSet amap
	where amap = processAllergens dat

doSolve :: M.Map String (S.Set String) -> S.Set String -> M.Map String (S.Set String)
doSolve amap todo
	|	S.null todo = amap
	| otherwise = if null candidates then error "No solution" else doSolve amap' todo'
	where
		candidates = [(k, v)|(k, vs) <- M.assocs amap, S.member k todo, S.size vs == 1, let [v] = S.elems vs]
		todo' = foldl (flip S.delete) todo $ map fst candidates
		amap' = M.mapWithKey removeCandidates amap
		removeCandidates k v = foldl (flip S.delete) v $ map snd $ filter ((/=k).fst) candidates

tests :: IO ()
tests = do
	check $ calcPartA testdata == 5
	check $ (intercalate "," $ M.elems $ solvePuzzle testdata) == "mxmxvkd,sqjhc,fvjkl"
	where
		testdata = map parseLine $ lines "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	dat <- getInput
	print $ calcPartA dat
	putStrLn $ intercalate "," $ M.elems $ solvePuzzle dat
