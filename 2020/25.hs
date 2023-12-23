{-# OPTIONS_GHC -Wno-tabs #-}
{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Maybe
import Control.Exception
import Modulo
import Utils

input = (1234567, 7654321)

magic_modulus = 20201227
base_value = 7

transform :: Integer -> Integer -> Integer
transform subject loop = toInteger $ (subject `modulo` magic_modulus) ^ loop

untransform :: Integer -> Integer -> Integer
untransform subject target = toInteger $ fromJust $ elemIndex (target `modulo` magic_modulus) $ iterate (*(subject `modulo` magic_modulus)) (1 `modulo` magic_modulus)

untransform2 :: Integer -> Integer -> Integer -> (Integer, Integer)
untransform2 subject target1 target2 = worker (1 `modulo` magic_modulus) 0
	where
		subject' = subject `modulo` magic_modulus
		target1' = target1 `modulo` magic_modulus
		target2' = target2 `modulo` magic_modulus
		worker !x !n
			| x == target1' = (1, n)
			| x == target2' = (2, n)
			| otherwise = worker (x * subject') (n + 1)

crack :: Integer -> Integer -> Integer
crack pubkey1 pubkey2 = case untransform2 7 pubkey1 pubkey2 of
	(1, k) -> transform pubkey2 k
	(2, k) -> transform pubkey1 k

tests :: IO ()
tests = do
	check $ transform 7 8 == 5764801
	check $ transform 7 11 == 17807724
	check $ untransform 7 5764801 == 8
	check $ untransform 7 17807724 == 11
	check $ untransform2 7 5764801 17807724 == (1, 8)
	check $ transform 5764801 11 == 14897079
	check $ transform 17807724 8 == 14897079
	check $ transform 7 (8*11) == 14897079
	check $ crack 5764801 17807724 == 14897079
	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	let (card_pub, door_pub) = input
	print $ crack card_pub door_pub
