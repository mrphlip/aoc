#!/usr/bin/python
from collections import defaultdict, Counter
from fractions import Fraction
from copy import deepcopy
from pprint import pprint
import re, math
import itertools

dat = """85513798[...]"""

#dat = "2333133121414131402"

disk = []
i = 0
toggle = True
for c in dat:
	if toggle:
		disk.extend([i] * int(c))
		i += 1
	else:
		disk.extend([None] * int(c))
	toggle = not toggle

i = 0
j = len(disk) - 1
while i < j:
	if disk[j] is None:
		j -= 1
	elif disk[i] is None:
		disk[i] = disk[j]
		i += 1
		j -= 1
	else:
		i += 1
disk = disk[:j+1]

print(sum(i*j for i,j in enumerate(disk)))

disk = []
#covered = {}
covered = set()
toggle = True
pos = 0
for c in dat:
	if toggle:
		disk.append([pos, int(c)])
		for i in range(pos,pos+int(c)):
			#covered[i] = len(disk) - 1
			covered.add(i)
	pos += int(c)
	toggle = not toggle

#def compact():
#	for i in range(len(disk) - 1, -1, -1):
#		for j in range(0, disk[i][0]):
#			#if all(k not in covered or covered[k] == i for k in range(j,j+disk[i][1])):
#			if all(k not in covered for k in range(j,j+disk[i][1])):
#				for k in range(disk[i][0], disk[i][0]+disk[i][1]):
#					del covered[k]
#				disk[i][0] = j
#				for k in range(disk[i][0], disk[i][0]+disk[i][1]):
#					covered[k] = i
#				return True
#	return False

#while compact():
#	pass

def compact(i):
	for j in range(0, disk[i][0]):
		#if all(k not in covered or covered[k] == i for k in range(j,j+disk[i][1])):
		if all(k not in covered for k in range(j,j+disk[i][1])):
			for k in range(disk[i][0], disk[i][0]+disk[i][1]):
				covered.remove(k)
			disk[i][0] = j
			for k in range(disk[i][0], disk[i][0]+disk[i][1]):
				covered.add(k)
			return

for i in range(len(disk) - 1, -1, -1):
	#print(i)
	compact(i)

#print(disk)
print(sum(i*l for i,(j,k) in enumerate(disk) for l in range(j,j+k)))
