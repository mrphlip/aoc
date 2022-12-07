{-# OPTIONS_GHC -Wno-tabs #-}
import Data.List
import qualified Data.Map.Strict as M
import Control.Exception

data Command = ChDirRoot | ChDirUp | ChDirDown String | Listing [Listing] deriving (Eq, Ord, Show, Read)
data Listing = DirEntry String | FileEntry String Integer deriving (Eq, Ord, Show, Read)
type Dirs = M.Map String Integer

getInput :: IO [Command]
getInput = do
	dat <- readFile "07.txt"
	return $ parseInput dat

parseInput :: String -> [Command]
parseInput s = map (uncurry parseCommand) $ splitCommands $ map words $ lines s
	where
		splitCommands [] = []
		splitCommands (x:xs) = (x, output) : splitCommands rest
			where (output, rest) = break ((=="$").head) xs

parseCommand :: [String] -> [[String]] -> Command
parseCommand ["$", "cd", "/"] [] = ChDirRoot
parseCommand ["$", "cd", ".."] [] = ChDirUp
parseCommand ["$", "cd", fn] [] = ChDirDown fn
parseCommand ["$", "ls"] xs = Listing $ map parseListing xs
parseListing :: [String] -> Listing
parseListing ["dir", fn] = DirEntry fn
parseListing [s, fn] = FileEntry fn $ read s

processCommands :: [Command] -> Dirs
processCommands = snd . foldl processCommand initState
	where
		initState = ([], M.empty)
		processCommand (_, dirs) ChDirRoot = ([], dirs)
		processCommand (_:p, dirs) ChDirUp = (p, dirs)
		processCommand (p, dirs) (ChDirDown fn) = (fn:p, dirs)
		processCommand (p, dirs) (Listing ls) = (p, addSize p (calcSize ls) dirs)
		addSize p n dirs = M.unionWith (+) dirs $ M.fromList [(intercalate "/" $ reverse p', n) | p' <- tails p]
		calcSize = sum . map calcListingSize
		calcListingSize (DirEntry _) = 0
		calcListingSize (FileEntry _ n) = n

partA :: Dirs -> Integer
partA dirs = sum $ filter (<=100000) $ M.elems dirs

partB :: Dirs -> Integer
partB dirs = minimum $ filter (>=target) $ M.elems dirs
	where target = 30000000 - (70000000 - (dirs M.! ""))

tests :: IO ()
tests = do
	check $ dirs == M.fromList [("a/e", 584), ("a", 94853), ("d", 24933642), ("", 48381165)]
	check $ partA dirs == 95437
	check $ partB dirs == 24933642
	where
		testData = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
		commands = parseInput testData
		dirs = processCommands commands
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main :: IO ()
main = do
	tests
	dat <- getInput
	let dirs = processCommands dat
	print $ partA dirs
	print $ partB dirs
