{-# OPTIONS_GHC -Wno-tabs #-}
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P
import Control.Exception
import Control.Monad
import Utils

type Point = (Integer, Integer, Integer)
type Rect = (Point, Point)
type Operation = (Rect, Bool)
type State = S.Set Rect

getInput :: IO [Operation]
getInput = do
	dat <- readFile "22.txt"
	return $ map readLine $ lines dat
readLine = runReadP $ do
	flag <- (P.string "on" >> return True) P.+++ (P.string "off" >> return False)
	P.skipSpaces
	(xmin,xmax) <- readRange 'x'
	P.char ','
	(ymin,ymax) <- readRange 'y'
	P.char ','
	(zmin,zmax) <- readRange 'z'
	return (((xmin,ymin,zmin), (xmax,ymax,zmax)), flag)
	where
		readInt = P.readS_to_P reads :: P.ReadP Integer
		readRange c = do
			P.char c
			P.char '='
			minval <- readInt
			P.string ".."
			maxval <- readInt
			return (minval, maxval)

overlap :: Rect -> Rect -> Bool
overlap ((xmin1, ymin1, zmin1), (xmax1, ymax1, zmax1)) ((xmin2, ymin2, zmin2), (xmax2, ymax2, zmax2)) = xmin1 <= xmax2 && xmin2 <= xmax1 && ymin1 <= ymax2 && ymin2 <= ymax1 && zmin1 <= zmax2 && zmin2 <= zmax1

issubset :: Rect -> Rect -> Bool
issubset ((xmin1, ymin1, zmin1), (xmax1, ymax1, zmax1)) ((xmin2, ymin2, zmin2), (xmax2, ymax2, zmax2)) = xmin1 <= xmin2 && xmax2 <= xmax1 && ymin1 <= ymin2 && ymax2 <= ymax1 && zmin1 <= zmin2 && zmax2 <= zmax1

partA :: [Operation] -> [Operation]
partA = map restrictTarget . filter overlapsTarget
	where
		overlapsTarget = overlap ((-50,-50,-50),(50,50,50)) . fst
		restrictTarget (((xmin,ymin,zmin), (xmax,ymax,zmax)), flag) = (((max xmin (-50), max ymin (-50), max zmin (-50)), (min xmax 50, min ymax 50, min zmax 50)), flag)

-- subtractRect r1 r2 = a list of rectangles which (a) are disjoint, and (b) their union is r1 - r2
subtractRect :: Rect -> Rect -> [Rect]
subtractRect ((xmin1, ymin1, zmin1), (xmax1, ymax1, zmax1)) ((xmin2, ymin2, zmin2), (xmax2, ymax2, zmax2)) = xbelow ++ xabove ++ ybelow ++ yabove ++ zbelow ++ zabove
	where
		xbelow = if xmin1 < xmin2 then [((xmin1, ymin1, zmin1), (xmin2-1, ymax1, zmax1))] else []
		xabove = if xmax1 > xmax2 then [((xmax2+1, ymin1, zmin1), (xmax1, ymax1, zmax1))] else []
		xmin' = max xmin1 xmin2
		xmax' = min xmax1 xmax2
		ybelow = if ymin1 < ymin2 then [((xmin', ymin1, zmin1), (xmax', ymin2-1, zmax1))] else []
		yabove = if ymax1 > ymax2 then [((xmin', ymax2+1, zmin1), (xmax', ymax1, zmax1))] else []
		ymin' = max ymin1 ymin2
		ymax' = min ymax1 ymax2
		zbelow = if zmin1 < zmin2 then [((xmin', ymin', zmin1), (xmax', ymax', zmin2-1))] else []
		zabove = if zmax1 > zmax2 then [((xmin', ymin', zmax2+1), (xmax', ymax', zmax1))] else []
		zmin' = max zmin1 zmin2
		zmax' = min zmax1 zmax2

subtractState :: State -> Rect -> State
subtractState state r = S.fromList $ join $ map doSubtract $ S.toList state
	where doSubtract r'
		| issubset r r' = []
		| overlap r r' = subtractRect r' r
		| otherwise = [r']

applyOperation :: State -> Operation -> State
applyOperation state (r, False) = subtractState state r
applyOperation state (r, True) = S.insert r $ subtractState state r

applyOperations :: [Operation] -> State
applyOperations = foldl applyOperation S.empty

rectSize :: Rect -> Integer
rectSize ((xmin,ymin,zmin), (xmax,ymax,zmax)) = (xmax - xmin + 1) * (ymax - ymin + 1) * (zmax - zmin + 1)

stateSize :: State -> Integer
stateSize = foldl (+) 0 . map rectSize . S.toList

tests :: IO ()
tests = do
	check $ (stateSize $ applyOperations $ partA sample1) == 39
	check $ (stateSize $ applyOperations $ partA sample2) == 590784
	check $ (stateSize $ applyOperations sample3) == 2758514936282235
	where
		sample1 = map readLine $ lines "on x=10..12,y=10..12,z=10..12\non x=11..13,y=11..13,z=11..13\noff x=9..11,y=9..11,z=9..11\non x=10..10,y=10..10,z=10..10"
		sample2 = map readLine $ lines "on x=-20..26,y=-36..17,z=-47..7\non x=-20..33,y=-21..23,z=-26..28\non x=-22..28,y=-29..23,z=-38..16\non x=-46..7,y=-6..46,z=-50..-1\non x=-49..1,y=-3..46,z=-24..28\non x=2..47,y=-22..22,z=-23..27\non x=-27..23,y=-28..26,z=-21..29\non x=-39..5,y=-6..47,z=-3..44\non x=-30..21,y=-8..43,z=-13..34\non x=-22..26,y=-27..20,z=-29..19\noff x=-48..-32,y=26..41,z=-47..-37\non x=-12..35,y=6..50,z=-50..-2\noff x=-48..-32,y=-32..-16,z=-15..-5\non x=-18..26,y=-33..15,z=-7..46\noff x=-40..-22,y=-38..-28,z=23..41\non x=-16..35,y=-41..10,z=-47..6\noff x=-32..-23,y=11..30,z=-14..3\non x=-49..-5,y=-3..45,z=-29..18\noff x=18..30,y=-20..-8,z=-3..13\non x=-41..9,y=-7..43,z=-33..15\non x=-54112..-39298,y=-85059..-49293,z=-27449..7877\non x=967..23432,y=45373..81175,z=27513..53682"
		sample3 = map readLine $ lines "on x=-5..47,y=-31..22,z=-19..33\non x=-44..5,y=-27..21,z=-14..35\non x=-49..-1,y=-11..42,z=-10..38\non x=-20..34,y=-40..6,z=-44..1\noff x=26..39,y=40..50,z=-2..11\non x=-41..5,y=-41..6,z=-36..8\noff x=-43..-33,y=-45..-28,z=7..25\non x=-33..15,y=-32..19,z=-34..11\noff x=35..47,y=-46..-34,z=-11..5\non x=-14..36,y=-6..44,z=-16..29\non x=-57795..-6158,y=29564..72030,z=20435..90618\non x=36731..105352,y=-21140..28532,z=16094..90401\non x=30999..107136,y=-53464..15513,z=8553..71215\non x=13528..83982,y=-99403..-27377,z=-24141..23996\non x=-72682..-12347,y=18159..111354,z=7391..80950\non x=-1060..80757,y=-65301..-20884,z=-103788..-16709\non x=-83015..-9461,y=-72160..-8347,z=-81239..-26856\non x=-52752..22273,y=-49450..9096,z=54442..119054\non x=-29982..40483,y=-108474..-28371,z=-24328..38471\non x=-4958..62750,y=40422..118853,z=-7672..65583\non x=55694..108686,y=-43367..46958,z=-26781..48729\non x=-98497..-18186,y=-63569..3412,z=1232..88485\non x=-726..56291,y=-62629..13224,z=18033..85226\non x=-110886..-34664,y=-81338..-8658,z=8914..63723\non x=-55829..24974,y=-16897..54165,z=-121762..-28058\non x=-65152..-11147,y=22489..91432,z=-58782..1780\non x=-120100..-32970,y=-46592..27473,z=-11695..61039\non x=-18631..37533,y=-124565..-50804,z=-35667..28308\non x=-57817..18248,y=49321..117703,z=5745..55881\non x=14781..98692,y=-1341..70827,z=15753..70151\non x=-34419..55919,y=-19626..40991,z=39015..114138\non x=-60785..11593,y=-56135..2999,z=-95368..-26915\non x=-32178..58085,y=17647..101866,z=-91405..-8878\non x=-53655..12091,y=50097..105568,z=-75335..-4862\non x=-111166..-40997,y=-71714..2688,z=5609..50954\non x=-16602..70118,y=-98693..-44401,z=5197..76897\non x=16383..101554,y=4615..83635,z=-44907..18747\noff x=-95822..-15171,y=-19987..48940,z=10804..104439\non x=-89813..-14614,y=16069..88491,z=-3297..45228\non x=41075..99376,y=-20427..49978,z=-52012..13762\non x=-21330..50085,y=-17944..62733,z=-112280..-30197\non x=-16478..35915,y=36008..118594,z=-7885..47086\noff x=-98156..-27851,y=-49952..43171,z=-99005..-8456\noff x=2032..69770,y=-71013..4824,z=7471..94418\non x=43670..120875,y=-42068..12382,z=-24787..38892\noff x=37514..111226,y=-45862..25743,z=-16714..54663\noff x=25699..97951,y=-30668..59918,z=-15349..69697\noff x=-44271..17935,y=-9516..60759,z=49131..112598\non x=-61695..-5813,y=40978..94975,z=8655..80240\noff x=-101086..-9439,y=-7088..67543,z=33935..83858\noff x=18020..114017,y=-48931..32606,z=21474..89843\noff x=-77139..10506,y=-89994..-18797,z=-80..59318\noff x=8476..79288,y=-75520..11602,z=-96624..-24783\non x=-47488..-1262,y=24338..100707,z=16292..72967\noff x=-84341..13987,y=2429..92914,z=-90671..-1318\noff x=-37810..49457,y=-71013..-7894,z=-105357..-13188\noff x=-27365..46395,y=31009..98017,z=15428..76570\noff x=-70369..-16548,y=22648..78696,z=-1892..86821\non x=-53470..21291,y=-120233..-33476,z=-44150..38147\noff x=-93533..-4276,y=-16170..68771,z=-104985..-24507"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	print $ stateSize $ applyOperations $ partA dat
	print $ stateSize $ applyOperations dat
