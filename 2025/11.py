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
def part1(node):
	if node == "you":
		return 1
	else:
		return sum(part1(i) for i in revdat[node])
print(part1("out"))

@cache
def part2(node, seenfft, seendac):
	if node == "svr":
		return 1 if seenfft and seendac else 0
	else:
		return sum(part2(i, seenfft or node == "fft", seendac or node == "dac") for i in revdat[node])
print(part2("out", False, False))
