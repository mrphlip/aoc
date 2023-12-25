#!/usr/bin/python
from collections import defaultdict
from copy import deepcopy
dat = {
	"zvk":["shb","cxf","qlp","jkz"],
	"mtl":["jvv","grn","dpt","gzc"],
	"rgl":["hfg"],
	[...snip...]
}

#dat = {"jqt":["rhn","xhk","nvd"], "rsh":["frs","pzl","lsr"], "xhk":["hfx"], "cmg":["qnr","nvd","lhk","bvb"], "rhn":["xhk","bvb","hfx"], "bvb":["xhk","hfx"], "pzl":["lsr","hfx","nvd"], "qnr":["nvd"], "ntq":["jqt","hfx","bvb","xhk"], "nvd":["lhk"], "lsr":["lhk"], "rzs":["qnr","cmg","lsr","rsh"], "frs":["qnr","lhk","lsr"]}

dat2 = defaultdict(lambda:defaultdict(lambda:0))
for k,v in dat.items():
	for i in v:
		dat2[k][i] += 1
		dat2[i][k] += 1

def mcp(G, a):
	A = {a}
	last = prev = a
	while len(A) < len(G):
		z = max(G.keys() - A, key=lambda i: sum(G[i][j] for j in G[i].keys() & A))
		A.add(z)
		last, prev = z, last
	cut_weight = sum(G[last].values())
	# merge last and prev
	G = deepcopy(G)
	newkey = f"{last}+{prev}"
	for i,w in G[last].items():
		if i != prev:
			G[newkey][i] += w
			G[i][newkey] += w
			G[i].pop(last, None)
	for i,w in G[prev].items():
		if i != last:
			G[newkey][i] += w
			G[i][newkey] += w
			G[i].pop(prev, None)
	del G[prev]
	del G[last]
	return cut_weight, last, G

def mc(G):
	a = next(iter(G))
	minval = None
	minat = None
	while len(G) > 1:
		print(len(G))
		cut_weight, last, G = mcp(G, a)
		if minval is None or cut_weight < minval:
			minval = cut_weight
			minat = last
	return minval, minat

minval, minat = mc(dat2)
print()
print(minval)
a = minat.count('+') + 1
b = len(dat2) - a
print(a, b)
print(a*b)
