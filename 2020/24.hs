{-# OPTIONS_GHC -Wno-tabs #-}
{-# LANGUAGE TupleSections #-}
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Exception
import Vector

data Step = East | SouthEast | SouthWest | West | NorthWest | NorthEast deriving (Eq, Show, Read)
allSteps = [East, SouthEast, SouthWest, West, NorthWest, NorthEast]

getInput :: IO [[Step]]
getInput = do
	dat <- readFile "24.txt"
	return $ map readSteps $ lines dat

readSteps :: String -> [Step]
readSteps [] = []
readSteps ('e':rest) = East : readSteps rest
readSteps ('s':'e':rest) = SouthEast : readSteps rest
readSteps ('s':'w':rest) = SouthWest : readSteps rest
readSteps ('w':rest) = West : readSteps rest
readSteps ('n':'w':rest) = NorthWest : readSteps rest
readSteps ('n':'e':rest) = NorthEast : readSteps rest

step :: Step -> Vector2 Integer
step East = Vector2 1 0
step West = Vector2 (-1) 0
step NorthEast = Vector2 0 1
step SouthWest = Vector2 0 (-1)
step NorthWest = Vector2 (-1) 1
step SouthEast = Vector2 1 (-1)

steps :: [Step] -> Vector2 Integer
steps = foldl (.+) (Vector2 0 0) . map step

getInitTiles :: [[Step]] -> S.Set (Vector2 Integer)
getInitTiles dat = M.keysSet $ M.filter id $ M.fromListWith (/=) $ map (,True) $ map steps dat

neighbours :: Vector2 Integer -> [Vector2 Integer]
neighbours p = map (p .+) $ map step allSteps

iterstep :: S.Set (Vector2 Integer) -> S.Set (Vector2 Integer)
iterstep tiles = (tiles `S.union` toFlipWhite) `S.difference` toFlipBlack
	where
		neighbourCount = M.fromListWith (+) ([(t, 0) | t <- S.toList tiles] ++ [(n,1) | t <- S.toList tiles, n <- neighbours t])
		toFlipBlack = S.filter shouldFlipBlack tiles
		toFlipWhite = S.filter shouldFlipWhite $ M.keysSet neighbourCount `S.difference` tiles
		shouldFlipBlack t = count == 0 || count > 2
			where count = neighbourCount M.! t
		shouldFlipWhite t = count == 2
			where count = neighbourCount M.! t

tests :: IO ()
tests = do
	check $ (steps $ readSteps "esew") == step SouthEast
	check $ (steps $ readSteps "nwwswee") == Vector2 0 0
	check $ (S.size $ getInitTiles testdata) == 10
	where
		testdata = map readSteps $ lines "sesenwnenenewseeswwswswwnenewsewsw\nneeenesenwnwwswnenewnwwsewnenwseswesw\nseswneswswsenwwnwse\nnwnwneseeswswnenewneswwnewseswneseene\nswweswneswnenwsewnwneneseenw\neesenwseswswnenwswnwnwsewwnwsene\nsewnenenenesenwsewnenwwwse\nwenwwweseeeweswwwnwwe\nwsweesenenewnwwnwsenewsenwwsesesenwne\nneeswseenwwswnwswswnw\nnenwswwsewswnenenewsenwsenwnesesenew\nenewnwewneswsewnwswenweswnenwsenwsw\nsweneswneswneneenwnewenewwneswswnese\nswwesenesewenwneswnwwneseswwne\nenesenwswwswneneswsenwnewswseenwsese\nwnwnesenesenenwwnenwsewesewsesesew\nnenewswnwewswnenesenwnesewesw\neneswnwswnwsenenwnwnwwseeswneewsenese\nneswnwewnwnwseenwseesewsenwsweewe\nwseweeenwnesenwwwswnew"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	dat <- getInput
	let inittiles = getInitTiles dat
	print $ S.size $ inittiles
	print $ S.size $ iterate iterstep inittiles !! 100
