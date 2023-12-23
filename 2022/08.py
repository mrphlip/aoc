#!/usr/bin/python3
a = [
	"110120112111001131321041300301301303441234124551121322251330313143000402010402222102132100122022010",
	"102022211221313002214040003422200133214443443344513144525425414312301403121303234303302002320112112",
	[...snip...]
]
#a = ["30373","25512","65332","33549","35390"]
a = [[int(j) for j in i] for i in a]
cx = len(a[0])
cy = len(a)
b = [[False for x in range(cx)] for y in range(cy)]
count = 0
for y in range(cy):
	cur = -1
	for x in range(cx):
		i = a[y][x]
		if i > cur:
			cur = i
			if not b[y][x]:
				b[y][x] = True
				count += 1
	cur = -1
	for x in range(cx-1,-1,-1):
		i = a[y][x]
		if i > cur:
			cur = i
			if not b[y][x]:
				b[y][x] = True
				count += 1
for x in range(cx):
	cur = -1
	for y in range(cy):
		i = a[y][x]
		if i > cur:
			cur = i
			if not b[y][x]:
				b[y][x] = True
				count += 1
	cur = -1
	for y in range(cy-1,-1,-1):
		i = a[y][x]
		if i > cur:
			cur = i
			if not b[y][x]:
				b[y][x] = True
				count += 1
print(count)


def val(x,y):
	target = a[y][x]
	up = 0
	for y0 in range(y-1,-1,-1):
		up += 1
		if a[y0][x] >= target:
			break
	down = 0
	for y0 in range(y+1,cy):
		down += 1
		if a[y0][x] >= target:
			break
	left = 0
	for x0 in range(x-1,-1,-1):
		left += 1
		if a[y][x0] >= target:
			break
	right = 0
	for x0 in range(x+1,cx):
		right += 1
		if a[y][x0] >= target:
			break
	return up * down * left * right
print(max(val(x,y) for x in range(1,cx-1) for y in range(1,cy-1)))
