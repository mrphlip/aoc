#!/usr/bin/python
from aocimports import *

dat = "brbwuur, bwuw, [...]"
dat2 = """buwugbgrgururgwrgrrugbwgrwurgbubrggruwugwgrwguuurwu
bwbrurbwgurggbbwbr[...]"""

#dat = "r, wr, b, g, bwu, rb, gb, br"
#dat2 = "brwrr\nbggr\ngbbr\nrrbgbr\nubwu\nbwurrg\nbrgr\nbbrgwb"

dat = dat.split(", ")
dat2 = dat2.split("\n")

a = re.compile("^(%s)*$" % "|".join(dat))
n = 0
for i in dat2:
	if a.match(i):
		n += 1
print(n)

@cache
def count(s):
	if not s:
		return 1
	n = 0
	for i in dat:
		if s.startswith(i):
			n += count(s[len(i):])
	return n

n = 0
for i in dat2:
	n += count(i)
print(n)
