dat = [
	(True,-6,41,-12,39,-10,42), (True,-33,13,-34,15,3,47), ...
]

regions = set()
def ol(min1,max1,min2,max2):
	if min1 < min2:
		yield (min1,min2-1)
	yield max(min1,min2), min(max1, max2)
	if max1 > max2:
		yield (max2+1, max1)
def proc(s,xmin,xmax,ymin,ymax,zmin,zmax):
	#if xmax < -50 or ymax < -50 or zmax < -50 or xmin > 50 or ymin > 50 or zmin > 50:
	#	return

	for xmin2, xmax2, ymin2, ymax2, zmin2, zmax2 in regions:
		if (xmin2, xmax2, ymin2, ymax2, zmin2, zmax2) == (xmin,xmax,ymin,ymax,zmin,zmax):
			break
		if xmin <= xmax2 and xmin2 <= xmax and ymin <= ymax2 and ymin2 <= ymax and zmin <= zmax2 and zmin2 <= zmax:
			regions.remove((xmin2,xmax2,ymin2,ymax2,zmin2,zmax2))
			for xmin3, xmax3 in ol(xmin2, xmax2, xmin, xmax):
				for ymin3, ymax3 in ol(ymin2, ymax2, ymin, ymax):
					for zmin3, zmax3 in ol(zmin2, zmax2, zmin, zmax):
						regions.add((xmin3, xmax3, ymin3, ymax3, zmin3, zmax3))
			for xmin3, xmax3 in ol(xmin, xmax, xmin2, xmax2):
				for ymin3, ymax3 in ol(ymin, ymax, ymin2, ymax2):
					for zmin3, zmax3 in ol(zmin, zmax, zmin2, zmax2):
						proc(s, xmin3, xmax3, ymin3, ymax3, zmin3, zmax3)
			return

	if s:
		regions.add((xmin,xmax,ymin,ymax,zmin,zmax))
	elif (xmin,xmax,ymin,ymax,zmin,zmax) in regions:
		regions.remove((xmin,xmax,ymin,ymax,zmin,zmax))
for i,x in enumerate(dat[:2]):
	print(f" * {i}: {len(regions)}")
	proc(*x)

import pprint
pprint.pprint(regions)
count = 0
for xmin, xmax, ymin, ymax, zmin, zmax in regions:
	count += (xmax-xmin+1) * (ymax-ymin+1) * (zmax-zmin+1)
print(count)
