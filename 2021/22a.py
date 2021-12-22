dat = [
	(True,-6,41,-12,39,-10,42), (True,-33,13,-34,15,3,47), ...
]

state = [[[False for z in range(101)] for y in range(101)] for x in range(101)]
count = 0
for s,xmin,xmax,ymin,ymax,zmin,zmax in dat:
	if xmax < -50 or ymax < -50 or zmax < -50 or xmin > 50 or ymin > 50 or zmin > 50:
		continue
	for x in range(xmin,xmax+1):
		for y in range(ymin,ymax+1):
			for z in range(zmin,zmax+1):
				if -50 <= x <= 50 and -50 <= y <= 50 and -50 <= z <= 50:
					if state[x][y][z] != s:
						state[x][y][z] = s
						if s:
							count += 1
						else:
							count -= 1
print(count)
