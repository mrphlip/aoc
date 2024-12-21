#!/usr/bin/python
from aocimports import *

dat = [
	"671A",
	[...]
]

#dat = ["029A","980A","179A","456A","379A"]

buttonlocs = ["789","456","123","_0A"]
buttonlocs = {c: (x,y) for y, row in enumerate(buttonlocs) for x, c in enumerate(row)}
buttonlocs2 = ["_^A","<v>"]
buttonlocs2 = {c: (x,y) for y, row in enumerate(buttonlocs2) for x, c in enumerate(row)}

@cache
def doroute(frompos, topos, depth, badrow):
	if frompos[0] < topos[0]:
		horiz = ">" * (topos[0] - frompos[0])
	elif frompos[0] > topos[0]:
		horiz = "<" * (frompos[0] - topos[0])
	else:
		horiz = ""
	if frompos[1] < topos[1]:
		vert = "v" * (topos[1] - frompos[1])
	elif frompos[1] > topos[1]:
		vert = "^" * (frompos[1] - topos[1])
	else:
		vert = ""

	if horiz == "":
		opts = [vert]
	elif vert == "":
		opts = [horiz]
	elif frompos[0] == 0 and topos[1] == badrow:
		opts = [horiz + vert]
	elif topos[0] == 0 and frompos[1] == badrow:
		opts = [vert + horiz]
	else:
		opts = [horiz + vert, vert + horiz]

	def checkopts():
		for i in opts:
			yield routecode(i + "A", depth - 1, True)
	return min(checkopts())

def routecode(code, depth, arrows):
	if depth <= 0:
		return len(code)
	locs = buttonlocs2 if arrows else buttonlocs
	badrow = 0 if arrows else 3
	pos = locs["A"]
	res = 0
	for i in code:
		newpos = locs[i]
		res += doroute(pos, newpos, depth, badrow)
		pos = newpos
	return res

def go(depth):
	n = 0
	for i in dat:
		a = routecode(i, depth, False)
		#print(f"{i}: {a}")
		n += int(i[:-1]) * a
	return n
print(go(3))
print(go(26))
