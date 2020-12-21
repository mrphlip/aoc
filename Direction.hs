module Direction (Direction(..), directions, reverseDirection, flipDirX, flipDirY, rotDirLeft, rotDirRight, step) where

data Direction = LeftDir | RightDir | UpDir | DownDir deriving (Eq, Enum, Ord, Show, Read)

directions = [UpDir, LeftDir, DownDir, RightDir]

reverseDirection UpDir = DownDir
reverseDirection DownDir = UpDir
reverseDirection LeftDir = RightDir
reverseDirection RightDir = LeftDir

flipDirX UpDir = UpDir
flipDirX DownDir = DownDir
flipDirX LeftDir = RightDir
flipDirX RightDir = LeftDir

flipDirY UpDir = DownDir
flipDirY DownDir = UpDir
flipDirY LeftDir = LeftDir
flipDirY RightDir = RightDir

rotDirLeft UpDir = LeftDir
rotDirLeft LeftDir = DownDir
rotDirLeft DownDir = RightDir
rotDirLeft RightDir = UpDir

rotDirRight UpDir = RightDir
rotDirRight LeftDir = UpDir
rotDirRight DownDir = LeftDir
rotDirRight RightDir = DownDir

step :: Direction -> (Integer,Integer) -> (Integer,Integer)
step LeftDir (x, y) = (x-1, y)
step RightDir (x, y) = (x+1, y)
step UpDir (x, y) = (x, y-1)
step DownDir (x, y) = (x, y+1)
