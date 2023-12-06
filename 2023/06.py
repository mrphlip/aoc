#!/usr/bin/python
from math import floor, ceil, sqrt

times = (45, 98, 83, 73)
distances = (295, 1734, 1278, 1210)

def solve(t, d):
	disc = sqrt(t*t - 4*d)
	x1 = (t - disc)/2
	x2 = (t + disc)/2
	return int(floor(x1)) + 1, int(ceil(x2))

res = 1
for t, d in zip(times, distances):
	a, b = solve(t, d)
	res *= b - a
print(res)

t = int(''.join(map(str, times)))
d = int(''.join(map(str, distances)))
a, b = solve(t, d)
print(b - a)
