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
def doroute(code, ix, pos, arrows):
	if ix >= len(code):
		return [""]
	locs = buttonlocs2 if arrows else buttonlocs
	bottom = 1 if arrows else 3
	c = code[ix]
	newpos = locs[c]
	if pos[0] < newpos[0]:
		horiz = ">" * (newpos[0] - pos[0])
	elif pos[0] > newpos[0]:
		horiz = "<" * (pos[0] - newpos[0])
	else:
		horiz = ""
	if pos[1] < newpos[1]:
		vert = "v" * (newpos[1] - pos[1])
	elif pos[1] > newpos[1]:
		vert = "^" * (pos[1] - newpos[1])
	else:
		vert = ""

	if horiz == "":
		opts = [vert]
	elif vert == "":
		opts = [horiz]
	elif pos[0] == 0 and newpos[1] == bottom:
		opts = [horiz + vert]
	elif newpos[0] == 0 and pos[1] == bottom:
		opts = [vert + horiz]
	else:
		opts = [horiz + vert, vert + horiz]

	res = []
	for i in opts:
		for j in doroute(code, ix+1, newpos, arrows):
			res.append(i + "A" + j)
	return res

def route(code, arrows):
	locs = buttonlocs2 if arrows else buttonlocs
	return doroute(code, 0, locs["A"], arrows)

def fullroute(code):
	for i in route(code, False):
		for j in route(i, True):
			for k in route(j, True):
				yield k

n = 0
for i in dat:
	a = min(fullroute(i), key=len)
	n += int(i[:-1]) * len(a)
print(n)
