#!/usr/bin/python
from aocimports import *

dat = [
	(97978,50277),
	(97978,51488),
	[...]
]

#dat = [(7,1),(11,1),(11,7),(9,7),(9,5),(2,5),(2,3),(7,3),]; dat = [*dat[1:], dat[0]]

print(max(
	(abs(x2-x1)+1) * (abs(y2-y1)+1)
	for x1, y1 in dat for x2, y2 in dat))

for i in range(len(dat)):
	if i % 2 == 0:
		assert dat[i][0] == dat[(i+1) % len(dat)][0]
	else:
		assert dat[i][1] == dat[(i+1) % len(dat)][1]

#uniq_x = {x for x,y in dat}
#uniq_y = {y for x,y in dat}
#assert not (uniq_x & {i+1 for i in uniq_x})
#assert not (uniq_y & {i+1 for i in uniq_y})
v_edges = [
	(x1, min(y1, y2), max(y1, y2))
	for i, (x1, y1) in enumerate(dat)
	for x2, y2 in [dat[(i+1)%len(dat)]]
	if i % 2 == 0
]
h_edges = [
	(min(x1, x2), max(x1, x2), y1)
	for i, (x1, y1) in enumerate(dat)
	for x2, y2 in [dat[(i+1)%len(dat)]]
	if i % 2 == 1
]
def is_valid(x1, y1, x2, y2):
	if x1 > x2:
		x1, x2 = x2, x1
	if y1 > y2:
		y1, y2 = y2, y1
	if any(
			x1 < ex < x2 and ey2 > y1 and ey1 < y2
			for ex, ey1, ey2 in v_edges):
		return False
	if any(
			y1 < ey < y2 and ex2 > x1 and ex1 < x2
			for ex1, ex2, ey in h_edges):
		return False
	return True

print(max(
	(abs(x2-x1)+1) * (abs(y2-y1)+1)
	for x1, y1 in dat for x2, y2 in dat
	if is_valid(x1, y1, x2, y2)))
