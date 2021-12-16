{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Char
import Data.List
import qualified Text.ParserCombinators.ReadP as P
import Control.Exception
import Control.Monad
import Utils

data Packet = Literal Integer | Sum [Packet] | Product [Packet] | Minimum [Packet] | Maximum [Packet] | GreaterThan Packet Packet | LessThan Packet Packet | EqualTo Packet Packet deriving (Eq, Show, Read)

getInput :: IO String
getInput = do
	dat <- readFile "16.txt"
	return $ hexToBits dat
hexToBits :: String -> String
hexToBits = join . map digitToBits
digitToBits :: Char -> String
digitToBits = padl '0' 4 . showBaseN 2 . digitToInt

-- parsePacket dat = (parsed packet, version total)
parsePacket :: String -> (Packet, Integer)
parsePacket = fst . head . P.readP_to_S readPacket
	where
		readNum :: Int -> P.ReadP Integer
		readNum len = readBaseN 2 <$> P.count len P.get
		readPacket :: P.ReadP (Packet, Integer)
		readPacket = do
			ver <- readNum 3
			typeid <- readNum 3
			if typeid == 4
				then do
					n <- readLiteral 0
					return (Literal n, ver)
				else do
					(subpackets, vertotal) <- readSubpackets
					return (buildOpPacket typeid subpackets, ver+vertotal)
		readLiteral :: Integer -> P.ReadP Integer
		readLiteral acc = do
			cont <- readNum 1
			val <- readNum 4
			let acc' = acc * 16 + val
			if cont == 1
				then readLiteral acc'
				else return acc'
		readSubpackets :: P.ReadP ([Packet], Integer)
		readSubpackets = do
			mode <- readNum 1
			vals <- if mode == 0
				then readSubpacketsByLength =<< readNum 15
				else readSubpacketsByCount =<< readNum 11
			let packets = map fst vals
			let vertotal = sum $ map snd vals
			return (packets, vertotal)
		readSubpacketsByLength :: Integer -> P.ReadP [(Packet, Integer)]
		readSubpacketsByLength 0 = return []
		readSubpacketsByLength n | n > 0 = do
			(dataread, val) <- P.gather readPacket
			(val:) <$> readSubpacketsByLength (n - genericLength dataread)
		readSubpacketsByCount :: Integer -> P.ReadP [(Packet, Integer)]
		readSubpacketsByCount n = P.count (fromInteger n) readPacket
buildOpPacket :: Integer -> [Packet] -> Packet
buildOpPacket 0 packets = Sum packets
buildOpPacket 1 packets = Product packets
buildOpPacket 2 packets = Minimum packets
buildOpPacket 3 packets = Maximum packets
buildOpPacket 5 [a,b] = GreaterThan a b
buildOpPacket 6 [a,b] = LessThan a b
buildOpPacket 7 [a,b] = EqualTo a b

evalPacket :: Packet -> Integer
evalPacket (Sum xs) = sum $ map evalPacket xs
evalPacket (Product xs) = product $ map evalPacket xs
evalPacket (Minimum xs) = minimum $ map evalPacket xs
evalPacket (Maximum xs) = maximum $ map evalPacket xs
evalPacket (Literal n) = n
evalPacket (GreaterThan a b) = if evalPacket a > evalPacket b then 1 else 0
evalPacket (LessThan a b) = if evalPacket a < evalPacket b then 1 else 0
evalPacket (EqualTo a b) = if evalPacket a == evalPacket b then 1 else 0

tests :: IO ()
tests = do
	check $ (snd $ parsePacket $ hexToBits "8A004A801A8002F478") == 16
	check $ (snd $ parsePacket $ hexToBits "620080001611562C8802118E34") == 12
	check $ (snd $ parsePacket $ hexToBits "C0015000016115A2E0802F182340") == 23
	check $ (snd $ parsePacket $ hexToBits "A0016C880162017C3686B18A3D4780") == 31
	check $ (evalPacket $ fst $ parsePacket $ hexToBits "C200B40A82") == 3
	check $ (evalPacket $ fst $ parsePacket $ hexToBits "04005AC33890") == 54
	check $ (evalPacket $ fst $ parsePacket $ hexToBits "880086C3E88112") == 7
	check $ (evalPacket $ fst $ parsePacket $ hexToBits "CE00C43D881120") == 9
	check $ (evalPacket $ fst $ parsePacket $ hexToBits "D8005AC2A8F0") == 1
	check $ (evalPacket $ fst $ parsePacket $ hexToBits "F600BC2D8F") == 0
	check $ (evalPacket $ fst $ parsePacket $ hexToBits "9C005AC2F8F0") == 0
	check $ (evalPacket $ fst $ parsePacket $ hexToBits "9C0141080250320F1802104A08") == 1
	where
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	let (packet, vertotal) = parsePacket dat
	print vertotal
	print $ evalPacket packet
