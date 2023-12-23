#!/usr/bin/python
from math import lcm
dat = [
	("RGT", ("HDG", "QJV")),
	("QDM", ("GPB", "SXG")),
	("DJN", ("TQD", "BQN")),
	[...snip...]
]
m = "LRRLRRRLRRLLLRLLRRLR[...snip...]"

#dat = [("AAA",("BBB", "CCC")), ("BBB",("DDD", "EEE")), ("CCC",("ZZZ", "GGG")), ("DDD",("DDD", "DDD")), ("EEE",("EEE", "EEE")), ("GGG",("GGG", "GGG")), ("ZZZ",("ZZZ", "ZZZ")),]
#m = "RL"

dd = dict(dat)

p = "AAA"
i = 0
while p != "ZZZ":
	d = m[i % len(m)]
	p = dd[p][d == "R"]
	i += 1
print(i)

p = list(i for i in dd if i[-1] == "A")
l = [[] for i in range(len(p))]
i = 0
while not all(len(i) == 2 for i in l):
	d = m[i % len(m)]
	p = [dd[j][d == "R"] for j in p]
	for j, x in enumerate(p):
		if len(l[j]) < 2 and x[-1] == "Z":
			l[j].append(i)
	i += 1
l2 = [b-a for a,b in l]
print(lcm(*l2))
