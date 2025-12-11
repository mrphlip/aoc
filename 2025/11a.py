#!/usr/bin/python
from aocimports import *

dat = {
	"lvy":{"oyl","fyu","wde","kxs"},
	"ozc":{"ziu","zyw","omn","kzo"},
	[...]
}

#dat = {"aaa":{"you","hhh"},"you":{"bbb","ccc"},"bbb":{"ddd","eee"},"ccc":{"ddd","eee","fff"},"ddd":{"ggg"},"eee":{"out"},"fff":{"out"},"ggg":{"out"},"hhh":{"ccc","fff","iii"},"iii":{"out"},}
#dat = {"svr":{"aaa","bbb"},"aaa":{"fft"},"fft":{"ccc"},"bbb":{"tty"},"tty":{"ccc"},"ccc":{"ddd","eee"},"ddd":{"hub"},"hub":{"fff"},"eee":{"dac"},"dac":{"fff"},"fff":{"ggg","hhh"},"ggg":{"out"},"hhh":{"out"},}

nodes = set(dat.keys()) | {"out"}

revdat = {k: set() for k in nodes}
for k, v in dat.items():
	for x in v:
		revdat[x].add(k)

@cache
def docount(target, node):
	if node == target:
		return 1
	else:
		return sum(docount(target, i) for i in revdat[node])
print(docount("you", "out"))
print(
	docount("svr", "fft") * docount("fft", "dac") * docount("dac", "out") +
	docount("svr", "dac") * docount("dac", "fft") * docount("fft", "out"))
