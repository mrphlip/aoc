#!/usr/bin/python
from aocimports import *

dat = """\
5336553644444345344544134246423443634474453456455433543434354444344554344336446734443434424442135474
2231552222211222122232222222153222143972321313222122221132199121111212232232222223322324232211141222
[...]
"""

#dat = "987654321111111 811111111111119 234234234234278 818181911112111"

dat = [[int(c) for c in row] for row in dat.split()]

#n = 0
#for row in dat:
#	digit1 = max(row[:-1])
#	digit1ix = row.index(digit1)
#	digit2 = max(row[digit1ix+1:])
#	val = digit1 * 10 + digit2
#	n += val
#print(n)


def maximise(row, n):
	digits = []
	ix = 0
	while n > 0:
		dig = max(row[ix:len(row)-n+1])
		digits.append(dig)
		ix = row.index(dig, ix) + 1
		n -= 1
	return int(''.join(map(str, digits)))
print(sum(maximise(row, 2) for row in dat))
print(sum(maximise(row, 12) for row in dat))
