{-# OPTIONS_GHC -Wno-tabs #-}
import Utils
import Control.Exception

data Operation = DealNew | DealIncrement Integer | Cut Integer deriving (Eq, Show, Read)
type Deck = (Integer, Integer, Integer) -- size, init, offset

getInput :: IO [Operation]
getInput = do
	dat <- readFile "22.txt"
	return $ parseInput dat

parseInput :: String -> [Operation]
parseInput = map parseLine . lines

parseLine :: String -> Operation
parseLine "deal into new stack" = DealNew
parseLine ('d':'e':'a':'l':' ':'w':'i':'t':'h':' ':'i':'n':'c':'r':'e':'m':'e':'n':'t':' ':rest) = DealIncrement $ read rest
parseLine ('c':'u':'t':' ':rest) = Cut $ read rest

newDeck :: Integer -> Deck
newDeck size = (size, 0, 1)

doOperation :: Operation -> Deck -> Deck
doOperation DealNew (size, init, ofs) = (size, (-init - 1) `mod` size, (-ofs) `mod` size)
doOperation (DealIncrement n) (size, init, ofs) = (size, (init * n) `mod` size, (ofs * n) `mod` size)
doOperation (Cut n) (size, init, ofs) = (size, (init - n) `mod` size, ofs)

doOperations :: [Operation] -> Deck -> Deck
doOperations = flip $ foldl (flip doOperation)

cardPosition :: Deck -> Integer -> Integer
cardPosition (size, init, ofs) n = (init + ofs * n) `mod` size

cardAt :: Deck -> Integer -> Integer
cardAt (size, init, ofs) n = ((n - init) * modRecip ofs size) `mod` size

addOperations :: Deck -> Deck -> Deck
addOperations (size, init, ofs) deck2@(size2, _, _) = assert (size == size2) $ (size, zeropos, newofs)
	where
		zeropos = cardPosition deck2 init
		onepos = cardPosition deck2 (init + ofs)
		newofs = (onepos - zeropos) `mod` size

doubleOperations deck = addOperations deck deck
multiplyOperations (size, _, _) 0 = newDeck size
multiplyOperations deck 1 = deck
multiplyOperations deck n
	| even n = multiplyOperations (doubleOperations deck) (n `div` 2)
	| odd n = addOperations (multiplyOperations (doubleOperations deck) (n `div` 2)) deck

tests :: IO ()
tests = do
	testShuffle "deal with increment 7\ndeal into new stack\ndeal into new stack" (10,0,7)
	testShuffle "cut 6\ndeal with increment 7\ndeal into new stack" (10,1,3)
	testShuffle "deal with increment 7\ndeal with increment 9\ncut -2" (10,2,3)
	testShuffle "deal into new stack\ncut -2\ndeal with increment 7\ncut 8\ncut -4\ndeal with increment 7\ncut 3\ndeal with increment 9\ndeal with increment 3\ncut -1" (10,7,7)
	where
		testShuffle s deck@(size,_,_) = test $ doOperations (parseInput s) (newDeck size) == deck

main :: IO ()
main = do
	ops <- getInput
	let deckA = doOperations ops $ newDeck 10007
	print $ cardPosition deckA 2019
	let deckB = doOperations ops $ newDeck 119315717514047
	let repDeckB = multiplyOperations deckB 101741582076661
	print $ cardAt repDeckB 2020
