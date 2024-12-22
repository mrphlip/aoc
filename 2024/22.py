#!/usr/bin/python
from aocimports import *

dat = [
	15996872,
	620419,
	[...]
]

#dat = [1,10,100,2024]

def step(n):
	#n = prune(mix(n, n*64))
	#n = prune(mix(n, n//32))
	#n = prune(mix(n, n*2048))
	n = mix(n, prune(n*64))
	n = mix(n, n//32)
	n = mix(n, prune(n*2048))
	return n
def mix(a, b):
	return a^b
def prune(n):
	return n % 16777216

n = 0
for i in dat:
	for j in range(2000):
		i = step(i)
	n += i
print(n)

def analyse(seed):
	res = {}
	last = seed % 10
	code = []
	for i in range(4):
		seed = step(seed)
		dig = seed % 10
		code.append(dig - last)
		last = dig
	for i in range(1997):
		ctup = tuple(code)
		if ctup not in res:
			res[ctup] = last
		seed = step(seed)
		dig = seed % 10
		code.pop(0)
		code.append(dig - last)
		last = dig
	return res

#dat = [1, 2, 3, 2024]

res = defaultdict(int)
for i in dat:
	for k, v in analyse(i).items():
		res[k] += v
print(max(res.values()))
