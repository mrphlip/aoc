{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Char
import Data.List
import Control.Exception

checkVal :: [Int] -> Bool
checkVal s = all id [
		length s == 6,
		all (>=0) s, all (<10) s,
		any (\(a,b) -> a == b) $ zip s (tail s),
		all (\(a,b) -> a <= b) $ zip s (tail s)
	]

checkValExt :: [Int] -> Bool
checkValExt s = any (\(a,b,c,d) -> (a /= b && b == c && c /= d)) $ zip4 padded (tail padded) (tail $ tail padded) (tail $ tail $ tail padded)
	where padded = [-1] ++ s ++ [-1]

digits :: Integer -> [Int]
digits x = map digitToInt $ prefixed
	where
		shown = show x
		prefixed = (replicate (6 - length shown) '0') ++ shown

tests = do
	check $ digits 123456 == [1,2,3,4,5,6]
	check $ digits 1234 == [0,0,1,2,3,4]

	check $ checkVal $ digits 111111
	check $ not $ checkVal $ digits 223450
	check $ not $ checkVal $ digits 123789

	check $ checkVal $ digits 112233
	check $ checkValExt $ digits 112233
	check $ checkVal $ digits 123444
	check $ not $ checkValExt $ digits 123444
	check $ checkVal $ digits 111122
	check $ checkValExt $ digits 111122
	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	let range = map digits $ [272091..815432]
	let candidates = filter checkVal range
	print $ length candidates
	let candidatesext = filter checkValExt candidates
	print $ length candidatesext
