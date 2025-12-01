#!/usr/bin/python
from aocimports import *

dat = """\
R32
R2
R4
[...]
"""

#dat = "L68 L30 R48 L5 R60 L55 L1 L99 R14 L82"

dat = [(i[0], int(i[1:])) for i in dat.split()]


pos = 50
count = 0
for dir, step in dat:
	if dir == "L":
		pos -= step
	elif dir == "R":
		pos += step
	else:
		1/0
	pos = pos % 100
	if pos == 0:
		count += 1
print(count)


pos = 50
count = 0
for dir, step in dat:
	prevpos = pos
	if dir == "L":
		pos -= step
		if pos < 0 and prevpos == 0:
			count -= 1
		while pos < 0:
			count += 1
			pos += 100
		if pos == 0:
			count += 1
	elif dir == "R":
		pos += step
		while pos >= 100:
			count += 1
			pos -= 100
	else:
		1/0
print(count)
