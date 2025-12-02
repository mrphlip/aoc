#!/usr/bin/python
from aocimports import *

dat = """\
52500467-52574194,[...]"""

#dat = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

dat = [(int(j),int(k)+1) for i in dat.split(",") for j,k in [i.split('-')]]

def iterbads(a, b, k=2):
	i = str(a)
	if len(i) % k == 0:
		n = int(i[:len(i)//k])
		if int(str(n)*k) < a:
			n += 1
	else:
		n = 10 ** (len(i) // k)
	while int(str(n)*k) < b:
		yield int(str(n)*k)
		n += 1

def iterbadsall(a, b):
	return {i for l in primes_to(len(str(b))+1) for i in iterbads(a, b, l)}

print(sum(i for a, b in dat for i in iterbads(a, b)))
print(sum(i for a, b in dat for i in iterbadsall(a, b)))
