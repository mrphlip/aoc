{-# OPTIONS_GHC -Wno-tabs #-}
import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe
import Control.Monad
import Control.Exception
import Utils

type Item = (String, Integer)
type Production = ([Item], [Item]) -- ([(reagent, amount)], [(product, amount)])
type Recipe = M.Map String Production

readInput :: IO Recipe
readInput = do
	dat <- readFile "14.txt"
	return $ processInput $ parseInput dat

parseInput :: String -> [Production]
parseInput = map parseInputLine . filter (not . null) . lines

-- I really should invest in learning a proper parser
parseInputLine :: String -> Production
parseInputLine line = fst $ head $ readsLine line
	where
		lstrip = dropWhile (==' ')
		readWord "" = ("", "")
		readWord (c:xs)
			| (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') = let (w,rest) = readWord xs in (c:w, rest)
			| otherwise = ("", c:xs)
		readsInt = reads :: ReadS Integer
		readsName :: ReadS String
		readsName s = do
			let s' = lstrip s
			guard $ not $ null s'
			let (name, rest) = readWord s'
			guard $ not $ null name
			return (name, rest)
		readsItem :: ReadS Item
		readsItem s = do
			(count, s') <- readsInt s
			(name, s'') <- readsName s'
			return ((name, count), s'')
		readsItems :: ReadS [Item]
		readsItems s = do
			(item, s') <- readsItem s
			let s'' = lstrip s'
			case listToMaybe s'' of
				Just ',' -> do
					(rest, s''') <- readsItems $ tail s''
					return (item:rest, s''')
				_ -> return ([item], s'')
		readsLine :: ReadS Production
		readsLine s = do
			(reagents, s') <- readsItems s
			let s'' = lstrip s'
			guard $ head s'' == '=' && head (tail s'') == '>'
			(products, s''') <- readsItems $ tail $ tail s''
			guard $ null $ lstrip s'''
			return ((reagents, products), "")

showLine :: Production -> String
showLine (reagents, productions) = intercalate ", " (map showItem reagents) ++ " => " ++ intercalate ", " (map showItem productions)
	where showItem (r, n) 	= show n ++ " " ++ r

processInput :: [Production] -> Recipe
processInput = M.fromListWithKey (\k->throw $ AssertionFailed $ "Multiple productions for the same product " ++ k) . map conv
	where conv p@(_, [(product, _)]) = (product, p)

simplify :: Production -> Production
simplify (reagents, products) = (newReagents, newProducts)
	where
		negateReagents = [ (r,-n) | (r,n) <- reagents ]
		asMap = M.fromListWith (+) (negateReagents ++ products)
		unMap = M.toList asMap
		newReagents = [ (r,-n) | (r,n) <- unMap, n < 0 ]
		newProducts = [ (r,n) | (r,n) <- unMap, n > 0 ]

expandOnce :: Recipe -> Production -> Maybe Production
expandOnce recipe (reagents, products) = listToMaybe $ do
	(r, ramount) <- reagents
	guard $ M.member r recipe
	let (newreag, [(_, newamount)]) = recipe M.! r
	let times = ramount `div` newamount + if ramount `mod` newamount == 0 then 0 else 1
	let newreagMult = [(r', n' * times) | (r', n') <- newreag]
	let newresultMult = [(r, newamount * times)]
	return $ simplify ((reagents ++ newreagMult), (products ++ newresultMult))

expandSteps :: Recipe -> Production -> [Production]
expandSteps recipe = unfoldr1 $ expandOnce recipe

fullExpand :: Recipe -> Production -> Production
fullExpand recipe = last . expandSteps recipe

findMax :: Recipe -> Integer -> (Integer, Production)
findMax recipe maxOre = (finalMax, runVal finalMax)
	where
		runVal n = fullExpand recipe ([("FUEL",n)], [("_FUEL_", n)])
		testVal n = let [("ORE", n')] = fst $ runVal n in n'
		binaryGrow n = if testVal (2*n) <= maxOre then binaryGrow (2*n) else (n, 2*n)
		(growmin, growmax) = binaryGrow 1
		binarySearch min max -- min is the highest known-good number, max is the lowest known-bad number
			|	min >= max - 1 = min
			| otherwise = let mid = (min + max) `div` 2 in if testVal mid <= maxOre then binarySearch mid max else binarySearch min mid
		finalMax = binarySearch growmin growmax

tests :: IO ()
tests = do
	test $ parseInputLine "10 ORE => 10 A" == ([("ORE", 10)], [("A", 10)])
	test $ parseInputLine "7 A, 1 E => 1 FUEL" == ([("A", 7), ("E", 1)], [("FUEL", 1)])
	let sample1 = "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL"
	test $ parseInput sample1 == [
			([("ORE",10)],[("A",10)]),
			([("ORE",1)],[("B",1)]),
			([("A",7),("B",1)],[("C",1)]),
			([("A",7),("C",1)],[("D",1)]),
			([("A",7),("D",1)],[("E",1)]),
			([("A",7),("E",1)],[("FUEL",1)])
		]
	testExampleA sample1 31

	testExampleA "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL" 165
	testExampleA "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT" 13312
	testExampleA "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n17 NVRVD, 3 JNWZP => 8 VPVL\n53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n22 VJHF, 37 MNCFX => 5 FWMGM\n139 ORE => 4 NVRVD\n144 ORE => 7 JNWZP\n5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n145 ORE => 6 MNCFX\n1 NVRVD => 8 CXFTF\n1 VJHF, 6 MNCFX => 4 RFSQX\n176 ORE => 6 VJHF" 180697
	testExampleA "171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX" 2210736

	testExampleB "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT" 82892753
	testExampleB "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n17 NVRVD, 3 JNWZP => 8 VPVL\n53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n22 VJHF, 37 MNCFX => 5 FWMGM\n139 ORE => 4 NVRVD\n144 ORE => 7 JNWZP\n5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n145 ORE => 6 MNCFX\n1 NVRVD => 8 CXFTF\n1 VJHF, 6 MNCFX => 4 RFSQX\n176 ORE => 6 VJHF" 5586022
	testExampleB "171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX" 460664

	where
		testExampleA s count = test $ reagents == [("ORE", count)]
			where
				recipe = processInput $ parseInput s
				(reagents, _) = fullExpand recipe (recipe M.! "FUEL")

		testExampleB s count = test $ finalMax == count
			where
				recipe = processInput $ parseInput s
				(finalMax, _) = findMax recipe 1000000000000				

main :: IO ()
main = do
	recipe <- readInput
	putStrLn $ showLine $ fullExpand recipe (recipe M.! "FUEL")
	putStrLn $ showLine $ snd $ findMax recipe 1000000000000
