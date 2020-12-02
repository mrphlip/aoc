module Direction (Direction(..), directions, reverseDirection, step) where

data Direction = LeftDir | RightDir | UpDir | DownDir deriving (Eq, Show, Read)

directions = [UpDir, LeftDir, DownDir, RightDir]

reverseDirection UpDir = DownDir
reverseDirection DownDir = UpDir
reverseDirection LeftDir = RightDir
reverseDirection RightDir = LeftDir

step :: Direction -> (Integer,Integer) -> (Integer,Integer)
step LeftDir (x, y) = (x-1, y)
step RightDir (x, y) = (x+1, y)
step UpDir (x, y) = (x, y-1)
step DownDir (x, y) = (x, y+1)
