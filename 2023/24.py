#!/usr/bin/python
from fractions import Fraction as F

dat = [
	((359781776524153, 312705660279075, 236728636905923 ),( -44, -125, 18)),
	((276481733510955, 270867065789660, 273768862611813 ),( 35, 20, 33)),
	[...snip...]
]

xmin, xmax = 200000000000000, 400000000000000
ymin, ymax = 200000000000000, 400000000000000

#dat = [((19, 13, 30 ),( -2,  1, -2)), ((18, 19, 22 ),( -1, -1, -2)), ((20, 25, 34 ),( -2, -2, -4)), ((12, 31, 28 ),( -1, -2, -1)), ((20, 19, 15 ),(  1, -5, -3)),]
#xmin, xmax = 7, 27
#ymin, ymax = 7, 27

def getintersect1(h1, h2):
	try:
		((x1,y1,_),(xv1,yv1,_)) = h1
		((x2,y2,_),(xv2,yv2,_)) = h2
		t1 = F((xv2*(y1 - y2) + x2*yv2 - x1*yv2), (yv2*xv1 - xv2*yv1))
		t2 = F((xv1*(y2 - y1) + x1*yv1 - x2*yv1), (yv1*xv2 - xv1*yv2))
		return x1+xv1*t1, y1+yv1*t1, t1, t2
	except ZeroDivisionError:
		return -1, -1, -1, -1

n = 0
for i in range(len(dat)):
	for j in range(i+1, len(dat)):
		x, y, t1, t2 = getintersect1(dat[i], dat[j])
		if xmin <= x <= xmax and ymin <= y <= ymax and t1 >= 0 and t2 >= 0:
			n += 1
print(n)

#print("solve ", end='')
#for i in dat[:3]:
#	((x,y,z),(xv,yv,zv)) = i
#	print(f"(x-({x}))(b-({yv})) = (y-({y}))(a-({xv})); (x-({x}))(c-({zv})) = (z-({z}))(a-({xv})); ", end='')
#print()

from scipy.optimize import fsolve
def equations(p):
	x, y, z, xv, yv, zv = p
	res = []
	for i in dat[1:4]:
		((x1,y1,z1),(xv1,yv1,zv1)) = i
		res.append((x-x1)*(yv-yv1) - (y-y1)*(xv-xv1))
		res.append((x-x1)*(zv-zv1) - (z-z1)*(xv-xv1))
	return res
x, y, z, xv, yv, zv = fsolve(equations, dat[0][0] + dat[0][1])
#print(f"{x=}")
#print(f"{y=}")
#print(f"{z=}")
#print(f"{xv=}")
#print(f"{yv=}")
#print(f"{zv=}")

print(int(round(x+y+z)))
