{-# OPTIONS_GHC -Wno-tabs #-}
import Data.Function
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Exception
import Control.Monad
import GHC.Base ((<|>))
import Utils
import Vector

type Vec = Vector3 Integer
type Fingerprint = M.Map (Vec) (Vec, Vec)
data Axis = PosX | NegX | PosY | NegY | PosZ | NegZ deriving (Eq, Ord, Show, Read)
type Orientation = (Axis, Axis, Axis)
type Alignment = (Orientation, Vec)
type PointCloud = ([Vec], [Vec])

getInput :: IO [PointCloud]
getInput = do
	dat <- readFile "19.txt"
	return $ parseInput dat
parseInput :: String -> [PointCloud]
parseInput = map parseScanner . splitOn "\n\n"
parseScanner :: String -> PointCloud
parseScanner s = (map parsePoint $ filter ((/="---").take 3) $ lines s, [zero])
parsePoint :: String -> Vec
parsePoint s = let [a,b,c] = splitOn "," s in Vector3 (read a) (read b) (read c)

minOverlap = 12
minFingerintOverlap = 50
maxDist = 1000
allOrients = [ (a,b,c) | (a,a') <- axes, (b,b') <- axes, a' /= b', (c,c') <- axes, a' /= c', b' /= c' ]
	where axes = [(PosX, 0), (NegX, 0), (PosY, 1), (NegY, 1), (PosZ, 2), (NegZ, 2)]

canonVec :: Vec -> Vec
canonVec (Vector3 a b c) = Vector3 a' b' c'
	where [a', b', c'] = sort [abs a, abs b, abs c]

fingerprint :: [Vec] -> Fingerprint
fingerprint vs = M.fromList $ do
	(ix, v1) <- zip [0..] vs
	v2 <- take ix vs
	return (canonVec $ v1 .- v2, (v1, v2))

chooseMerge :: [Fingerprint] -> [(Integer, Integer)]
chooseMerge fingerprints = map fst $ reverse $ sortOn snd $ filter ((>=minFingerintOverlap).snd) overlaps
	where overlaps = [((ix2, ix1), S.size $ M.keysSet f1 `S.intersection` M.keysSet f2) | (ix1, f1) <- zip [0..] fingerprints, (ix2, f2) <- zip [0..] $ genericTake ix1 fingerprints ]

getAxis :: Axis -> Vec -> Integer
getAxis PosX (Vector3 x y z) = x
getAxis NegX (Vector3 x y z) = -x
getAxis PosY (Vector3 x y z) = y
getAxis NegY (Vector3 x y z) = -y
getAxis PosZ (Vector3 x y z) = z
getAxis NegZ (Vector3 x y z) = -z

rotateVec :: Orientation -> Vec -> Vec
rotateVec (a,b,c) v = Vector3 (getAxis a v) (getAxis b v) (getAxis c v)

transformVec :: Alignment -> Vec -> Vec
transformVec (o, a) v = rotateVec o v .+ a

alignPair :: Vec -> Vec -> Vec -> Vec -> [Alignment]
alignPair a1 a2 b1 b2 = do
	o <- allOrients
	let aff = a1 .- rotateVec o b1
	guard $ a2 == transformVec (o,aff) b2
	return (o,aff)

alignScanner :: [Vec] -> [Vec] -> Fingerprint -> Fingerprint -> [Alignment]
alignScanner s1 s2 f1 f2 = map fst $ reverse $ sortOn snd $ filter ((>1).snd) $ M.assocs candidateCount
	where
		candidates = do
			f <- S.toList $ M.keysSet f1 `S.intersection` M.keysSet f2
			let (a1, a2) = f1 M.! f
			let (b1, b2) = f2 M.! f
			alignPair a1 a2 b1 b2
		candidateCount = counter candidates

tryMergeAlign :: PointCloud -> PointCloud -> Alignment -> Maybe PointCloud
tryMergeAlign (s1,p1) (s2,p2) a = if length (s1 `intersect` transformed) >= minOverlap then Just (s1 `union` transformed, p1 ++ map (transformVec a) p2) else Nothing
	where transformed = map (transformVec a) s2

tryMergeScanners :: PointCloud -> PointCloud -> Fingerprint -> Fingerprint -> Maybe PointCloud
tryMergeScanners s1 s2 f1 f2 = foldr (<|>) Nothing $ map (tryMergeAlign s1 s2) $ alignScanner (fst s1) (fst s2) f1 f2

tryMerge :: [PointCloud] -> [Fingerprint] -> (Integer, Integer) -> Maybe [PointCloud]
tryMerge scanners fingerprints (ix1, ix2) = result
	where
		s1 = scanners `genericIndex` ix1
		s2 = scanners `genericIndex` ix2
		f1 = fingerprints `genericIndex` ix1
		f2 = fingerprints `genericIndex` ix2
		mergeResult = tryMergeScanners s1 s2 f1 f2
		before = genericTake ix1 scanners
		between = genericTake (ix2-ix1-1) $ genericDrop (ix1+1) scanners
		after = genericDrop (ix2+1) scanners
		rest = before ++ between ++ after
		result = case mergeResult of
			(Just merged) -> Just $ merged:rest
			Nothing -> Nothing

oneMerge :: [PointCloud] -> [PointCloud]
oneMerge scanners = fromJust $ foldr (<|>) Nothing $ map (tryMerge scanners fingerprints) $ chooseMerge fingerprints
	where fingerprints = map fingerprint $ map fst scanners

fullMerge :: [PointCloud] -> PointCloud
fullMerge [a] = a
fullMerge as@(_:_) = fullMerge $ oneMerge as

manhattanSize :: Vec -> Integer
manhattanSize (Vector3 x y z) = abs x + abs y + abs z
furthest :: [Vec] -> Integer
furthest ps = maximum $ [ manhattanSize (x .- y) | x <- ps, y <- ps ]

tests :: IO ()
tests = do
	check $ (length $ merged) == 79
	check $ (furthest scannerloc) == 3621
	where
		sample = parseInput "--- scanner 0 ---\n404,-588,-901\n528,-643,409\n-838,591,734\n390,-675,-793\n-537,-823,-458\n-485,-357,347\n-345,-311,381\n-661,-816,-575\n-876,649,763\n-618,-824,-621\n553,345,-567\n474,580,667\n-447,-329,318\n-584,868,-557\n544,-627,-890\n564,392,-477\n455,729,728\n-892,524,684\n-689,845,-530\n423,-701,434\n7,-33,-71\n630,319,-379\n443,580,662\n-789,900,-551\n459,-707,401\n\n--- scanner 1 ---\n686,422,578\n605,423,415\n515,917,-361\n-336,658,858\n95,138,22\n-476,619,847\n-340,-569,-846\n567,-361,727\n-460,603,-452\n669,-402,600\n729,430,532\n-500,-761,534\n-322,571,750\n-466,-666,-811\n-429,-592,574\n-355,545,-477\n703,-491,-529\n-328,-685,520\n413,935,-424\n-391,539,-444\n586,-435,557\n-364,-763,-893\n807,-499,-711\n755,-354,-619\n553,889,-390\n\n--- scanner 2 ---\n649,640,665\n682,-795,504\n-784,533,-524\n-644,584,-595\n-588,-843,648\n-30,6,44\n-674,560,763\n500,723,-460\n609,671,-379\n-555,-800,653\n-675,-892,-343\n697,-426,-610\n578,704,681\n493,664,-388\n-671,-858,530\n-667,343,800\n571,-461,-707\n-138,-166,112\n-889,563,-600\n646,-828,498\n640,759,510\n-630,509,768\n-681,-892,-333\n673,-379,-804\n-742,-814,-386\n577,-820,562\n\n--- scanner 3 ---\n-589,542,597\n605,-692,669\n-500,565,-823\n-660,373,557\n-458,-679,-417\n-488,449,543\n-626,468,-788\n338,-750,-386\n528,-832,-391\n562,-778,733\n-938,-730,414\n543,643,-506\n-524,371,-870\n407,773,750\n-104,29,83\n378,-903,-323\n-778,-728,485\n426,699,580\n-438,-605,-362\n-469,-447,-387\n509,732,623\n647,635,-688\n-868,-804,481\n614,-800,639\n595,780,-596\n\n--- scanner 4 ---\n727,592,562\n-293,-554,779\n441,611,-461\n-714,465,-776\n-743,427,-804\n-660,-479,-426\n832,-632,460\n927,-485,-438\n408,393,-506\n466,436,-512\n110,16,151\n-258,-428,682\n-393,719,612\n-211,-452,876\n808,-476,-593\n-575,615,604\n-485,667,467\n-680,325,-822\n-627,-443,-432\n872,-547,-609\n833,512,582\n807,604,487\n839,-516,451\n891,-625,532\n-652,-548,-490\n30,-46,-14"
		(merged, scannerloc) = fullMerge sample
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	scanners <- getInput
	let (merged, scannerloc) = fullMerge scanners
	print $ length merged
	print $ furthest scannerloc
