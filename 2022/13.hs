{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import Control.Exception
import Utils

data Packet = ListVal [Packet] | IntVal Integer deriving Eq

getInput :: IO [Packet]
getInput = map read <$> filter (not.null) <$> lines <$> readFile "13.txt"

instance (Read Packet) where
	readsPrec d r = [(ListVal x, rest) | (x, rest) <- readsPrec d r] ++ [(IntVal x, rest) | (x, rest) <- readsPrec d r]

instance (Show Packet) where
	showsPrec d (ListVal x) = showsPrec d x
	showsPrec d (IntVal x) = showsPrec d x

instance (Ord Packet) where
	compare (IntVal a) (IntVal b) = compare a b
	compare (ListVal a) (ListVal b) = compare a b
	compare a@(IntVal _) (ListVal b) = compare [a] b
	compare (ListVal a) b@(IntVal _) = compare a [b]

partA ps = sum [i | (i, [a, b]) <- zip [1..] $ chunk 2 ps, a < b]
partB ps = (index1 + 1) * (index2 + 1)
	where
		sentinel1 = ListVal [ListVal [IntVal 2]]
		sentinel2 = ListVal [ListVal [IntVal 6]]
		sorted = sort $ [sentinel1, sentinel2] ++ ps
		Just index1 = elemIndex sentinel1 sorted
		Just index2 = elemIndex sentinel2 sorted

tests :: IO ()
tests = do
	check $ partA testData == 13
	check $ partB testData == 140
	where
		testData = map read ["[1,1,3,1,1]", "[1,1,5,1,1]", "[[1],[2,3,4]]", "[[1],4]", "[9]", "[[8,7,6]]", "[[4,4],4,4]", "[[4,4],4,4,4]", "[7,7,7,7]", "[7,7,7]", "[]", "[3]", "[[[]]]", "[[]]", "[1,[2,[3,[4,[5,6,7]]]],8,9]", "[1,[2,[3,[4,[5,6,0]]]],8,9]"]
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	print $ partA dat
	print $ partB dat
