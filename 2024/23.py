#!/usr/bin/python
from aocimports import *

dat = [
	("qs","sa"),
	("sj","su"),
	[...]
]

#dat = [("kh","tc"),("qp","kh"),("de","cg"),("ka","co"),("yn","aq"),("qp","ub"),("cg","tb"),("vc","aq"),("tb","ka"),("wh","tc"),("yn","cg"),("kh","ub"),("ta","co"),("de","co"),("tc","td"),("tb","wq"),("wh","td"),("ta","ka"),("td","qp"),("aq","cg"),("wq","ub"),("ub","vc"),("de","ta"),("wq","aq"),("wq","vc"),("wh","yn"),("ka","de"),("kh","ta"),("co","tc"),("wh","qp"),("tb","vc"),("td","yn")]

conn = {(i,j) for i,j in dat} | {(j,i) for i,j in dat}
neighbours = defaultdict(set)
for i, j in dat:
	neighbours[i].add(j)
	neighbours[j].add(i)
comps = sorted(neighbours.keys())

n = 0
for i in comps:
	for j in neighbours[i]:
		if j > i:
			for k in neighbours[i] & neighbours[j]:
				if k > j:
					if i.startswith("t") or j.startswith("t") or k.startswith("t"):
						n += 1
print(n)

def cliques(incl, ix):
	if ix >= len(comps):
		yield incl
		return
	if all((i, comps[ix]) in conn for i in incl):
		yield from cliques(incl | {comps[ix]}, ix+1)
	yield from cliques(incl, ix+1)

cl = max(cliques(set(), 0), key=len)
print(",".join(sorted(cl)))
