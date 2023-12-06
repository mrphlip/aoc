#!/usr/bin/python
# Alternate solution using the brute-force strategy, so I could compare speeds

times = (45, 98, 83, 73)
distances = (295, 1734, 1278, 1210)

def solve(t, d):
	n = 0
	for i in range(t + 1):
		if i * (t - i) > d:
			n += 1
	return n

res = 1
for t, d in zip(times, distances):
	res *= solve(t, d)
print(res)

t = int(''.join(map(str, times)))
d = int(''.join(map(str, distances)))
print(solve(t, d))
