{-# OPTIONS_GHC -Wno-tabs #-}
import Control.Exception
import Data.Char
import Data.Maybe
import Data.List.Split (splitOn)
import Text.Read

data Passport = Passport (Maybe String) (Maybe String) (Maybe String) (Maybe String) (Maybe String) (Maybe String) (Maybe String) (Maybe String) deriving (Eq, Show, Read)

emptyPassport = Passport Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

addField :: Passport -> String -> Passport
addField (Passport byr iyr eyr hgt hcl ecl pid cid) ('b':'y':'r':':':val) = Passport (Just val) iyr eyr hgt hcl ecl pid cid
addField (Passport byr iyr eyr hgt hcl ecl pid cid) ('i':'y':'r':':':val) = Passport byr (Just val) eyr hgt hcl ecl pid cid
addField (Passport byr iyr eyr hgt hcl ecl pid cid) ('e':'y':'r':':':val) = Passport byr iyr (Just val) hgt hcl ecl pid cid
addField (Passport byr iyr eyr hgt hcl ecl pid cid) ('h':'g':'t':':':val) = Passport byr iyr eyr (Just val) hcl ecl pid cid
addField (Passport byr iyr eyr hgt hcl ecl pid cid) ('h':'c':'l':':':val) = Passport byr iyr eyr hgt (Just val) ecl pid cid
addField (Passport byr iyr eyr hgt hcl ecl pid cid) ('e':'c':'l':':':val) = Passport byr iyr eyr hgt hcl (Just val) pid cid
addField (Passport byr iyr eyr hgt hcl ecl pid cid) ('p':'i':'d':':':val) = Passport byr iyr eyr hgt hcl ecl (Just val) cid
addField (Passport byr iyr eyr hgt hcl ecl pid cid) ('c':'i':'d':':':val) = Passport byr iyr eyr hgt hcl ecl pid (Just val)
addField _ field = error $ "Bad field type - " ++ field

getInput :: IO [Passport]
getInput = do
	dat <- readFile "04.txt"
	return $ parseInput dat

parseInput :: String -> [Passport]
parseInput = map parsePassport . splitOn "\n\n"

parsePassport :: String -> Passport
parsePassport = foldl addField emptyPassport . words

validA :: Passport -> Bool
validA (Passport (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) _) = True
validA _ = False

validB :: Passport -> Bool
validB (Passport (Just byr) (Just iyr) (Just eyr) (Just hgt) (Just hcl) (Just ecl) (Just pid) _) = validByr byr && validIyr iyr && validEyr eyr && validHgt hgt && validHcl hcl && validEcl ecl && validPid pid
validB _ = False

validByr byr = maybe False checkval $ readMaybe byr
	where checkval x = 1920 <= x && x <= 2002
validIyr iyr = maybe False checkval $ readMaybe iyr
	where checkval x = 2010 <= x && x <= 2020
validEyr eyr = maybe False checkval $ readMaybe eyr
	where checkval x = 2020 <= x && x <= 2030
validHgt hgt = maybe False (checkval unit) $ readMaybe value
	where
		(value, unit) = splitAt (length hgt - 2) hgt
		checkval "in" x = 59 <= x && x <= 76
		checkval "cm" x = 150 <= x && x <= 193
		checkval _ _ = False
validHcl ('#':hcl) = all isHexDigit hcl && length hcl == 6
validHcl _ = False
validEcl "amb" = True
validEcl "blu" = True
validEcl "brn" = True
validEcl "gry" = True
validEcl "grn" = True
validEcl "hzl" = True
validEcl "oth" = True
validEcl _ = False
validPid pid = all isDigit pid && length pid == 9

tests :: IO ()
tests = do
	check $ map validA passportsA == [True, False, True, False]
	check $ validByr "2002"
	check $ not $ validByr "2003"
	check $ validHgt "60in"
	check $ validHgt "190cm"
	check $ not $ validHgt "190in"
	check $ not $ validHgt "190"
	check $ validHcl "#123abc"
	check $ not $ validHcl "#123abz"
	check $ not $ validHcl "123abc"
	check $ validEcl "brn"
	check $ not $ validEcl "wat"
	check $ validPid "000000001"
	check $ not $ validPid "0123456789"
	check $ all (not . validB) passportsBinvalid
	check $ all validB passportsBvalid
	where
		passportsA = parseInput "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"
		passportsBinvalid = parseInput "eyr:1972 cid:100\nhcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\niyr:2019\nhcl:#602927 eyr:1967 hgt:170cm\necl:grn pid:012533040 byr:1946\n\nhcl:dab227 iyr:2012\necl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\nhgt:59cm ecl:zzz\neyr:2038 hcl:74454a iyr:2023\npid:3556412378 byr:2007"
		passportsBvalid = parseInput "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f\n\neyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\nhcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022\n\niyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
		check True = return ()
		check False = throwIO $ AssertionFailed "test failed"

main = do
	passports <- getInput
	print $ length $ filter validA passports
	print $ length $ filter validB passports
