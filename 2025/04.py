#!/usr/bin/python
from aocimports import *

dat = """\
@@@@@@.@@..@@.@.@@.@@...@@.@@@.@.@.@.@@.@@@@@@@@@@@@.@.@@.@.@...@.@@@@@@@@..@@..@@@..@@@.@@@@..@.....@@@.@@.@@@@@@.@@.@@.@.@@@@@@.@@.@@.@
..@@.@@@@.@.@.@.@.@@..@@.@@@@@@.@@@..@@.@@.@.@.@.@.....@@..@.@@@@...@@@.@.@@.@..@.@@@@@..@..@@@@.@@@@.@@@@@@@@@@@@@@.@@..@.@.@..@.@@@@@@@
[...]"""

#dat = "..@@.@@@@. @@@.@.@.@@ @@@@@.@.@@ @.@@@@..@. @@.@@@@.@@ .@@@@@@@.@ .@.@.@.@@@ @.@@@.@@@@ .@@@@@@@@. @.@.@@@.@."

dat = [[{"@":True,".":False}[c] for c in row] for row in dat.split()]
rolls = {(x, y) for y, row in enumerate(dat) for x, c in enumerate(row) if c}

def removable(rolls):
	return [(x,y) for x, y in rolls if len({(x+dx,y+dy) for dx in [-1,0,1] for dy in [-1,0,1]} & rolls) <= 4]
print(len(removable(rolls)))

remain = rolls
while True:
	remove = set(removable(remain))
	if not remove:
		break
	remain = remain - remove
print(len(rolls) - len(remain))
