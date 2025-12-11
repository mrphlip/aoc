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

print ("digraph {")
for k in nodes:
	col = {"you": "blue", "svr": "blue", "fft": "red", "dac": "red", "out": "green"}.get(k)
	if col:
		print(f"{k} [color={col}];")
	else:
		print(k)
for k, vs in dat.items():
	for v in vs:
		print(f"{k} -> {v};")
print("}")
