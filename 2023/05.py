#!/usr/bin/python
seeds = [91926764,235794528,3279509610,[...snip...]]
maps = [
	[
	(2076625497,3385713231,258448094),
	(933162806,1124446801,128749435),
	(744984268,625015359,188178538),
	[...snip...]
	],[
	[...snip...]
	]
]

#seeds = [79,14,55,13]
#maps = [[(50,98,2), (52,50,48), ],[(0,15,37), (37,52,2), (39,0,15), ],[(49,53,8), (0,11,42), (42,0,7), (57,7,4), ],[(88,18,7), (18,25,70), ],[(45,77,23), (81,45,19), (68,64,13), ],[(0,69,1), (1,0,69), ],[(60,56,37), (56,93,4), ]]


def domap(m, v):
	for dst, src, n in m:
		if src <= v < src + n:
			return v - src + dst
	return v
s = list(seeds)
for m in maps:
	s = [domap(m, i) for i in s]
print(min(s))

def maprange(m, a, b):
	for dst, src, n in m:
		if src < b and a < src + n:
			ra = max(src, a)
			rb = min(src+n, b)
			yield (ra - src + dst, rb - src + dst)
			if a < src:
				yield from maprange(m, a, src)
			if b > src + n:
				yield from maprange(m, src+n, b)
			return
	yield (a, b)
	return
def flatten(r):
	new = []
	r.sort()
	for a, b in r:
		if new and a <= new[-1][1]:
			new[-1][1] = max(new[-1][1], b)
		else:
			new.append([a,b])
	return new
def mapranges(m, s):
	new = []
	for r in s:
		new.extend(maprange(m, *r))
	return flatten(new)
s = [(seeds[i], seeds[i] + seeds[i+1]) for i in range(0, len(seeds), 2)]
for m in maps:
	s = mapranges(m, s)
print(s[0][0])
