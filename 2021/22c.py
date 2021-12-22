dat = [
	(True,-6,41,-12,39,-10,42), (True,-33,13,-34,15,3,47), ...
]

regions = set()
def proc(s,xmin,xmax,ymin,ymax,zmin,zmax):
	#if xmax < -50 or ymax < -50 or zmax < -50 or xmin > 50 or ymin > 50 or zmin > 50:
	#	return

	for xmin2, xmax2, ymin2, ymax2, zmin2, zmax2 in regions:
		if (xmin2, xmax2, ymin2, ymax2, zmin2, zmax2) == (xmin,xmax,ymin,ymax,zmin,zmax):
			break
		if xmin <= xmax2 and xmin2 <= xmax and ymin <= ymax2 and ymin2 <= ymax and zmin <= zmax2 and zmin2 <= zmax:
			regions.remove((xmin2,xmax2,ymin2,ymax2,zmin2,zmax2))

			if xmin2 < xmin:
				regions.add((xmin2,xmin-1,ymin2,ymax2,zmin2,zmax2))
			if xmax2 > xmax:
				regions.add((xmax+1,xmax2,ymin2,ymax2,zmin2,zmax2))
			if ymin2 < ymin:
				regions.add((max(xmin,xmin2),min(xmax,xmax2),ymin2,ymin-1,zmin2,zmax2))
			if ymax2 > ymax:
				regions.add((max(xmin,xmin2),min(xmax,xmax2),ymax+1,ymax2,zmin2,zmax2))
			if zmin2 < zmin:
				regions.add((max(xmin,xmin2),min(xmax,xmax2),max(ymin,ymin2),min(ymax,ymax2),zmin2,zmin-1))
			if zmax2 > zmax:
				regions.add((max(xmin,xmin2),min(xmax,xmax2),max(ymin,ymin2),min(ymax,ymax2),zmax+1,zmax2))
			regions.add((max(xmin,xmin2),min(xmax,xmax2),max(ymin,ymin2),min(ymax,ymax2),max(zmin,zmin2),min(zmax,zmax2)))

			if xmin < xmin2:
				proc(s,xmin,xmin2-1,ymin,ymax,zmin,zmax)
			if xmax > xmax2:
				proc(s,xmax2+1,xmax,ymin,ymax,zmin,zmax)
			if ymin < ymin2:
				proc(s,max(xmin,xmin2),min(xmax,xmax2),ymin,ymin2-1,zmin,zmax)
			if ymax > ymax2:
				proc(s,max(xmin,xmin2),min(xmax,xmax2),ymax2+1,ymax,zmin,zmax)
			if zmin < zmin2:
				proc(s,max(xmin,xmin2),min(xmax,xmax2),max(ymin,ymin2),min(ymax,ymax2),zmin,zmin2-1)
			if zmax > zmax2:
				proc(s,max(xmin,xmin2),min(xmax,xmax2),max(ymin,ymin2),min(ymax,ymax2),zmax2+1,zmax)
			proc(s,max(xmin,xmin2),min(xmax,xmax2),max(ymin,ymin2),min(ymax,ymax2),max(zmin,zmin2),min(zmax,zmax2))

			return

	if s:
		regions.add((xmin,xmax,ymin,ymax,zmin,zmax))
	elif (xmin,xmax,ymin,ymax,zmin,zmax) in regions:
		regions.remove((xmin,xmax,ymin,ymax,zmin,zmax))
for i,x in enumerate(dat):
	print(f" * {i}: {len(regions)}")
	proc(*x)

count = 0
for xmin, xmax, ymin, ymax, zmin, zmax in regions:
	count += (xmax-xmin+1) * (ymax-ymin+1) * (zmax-zmin+1)
print(count)
