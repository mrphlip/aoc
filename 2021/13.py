points = [(6,10), (0,14), (9,10), (0,3), (10,4), (4,11), (6,0), (6,12), (4,1), (0,13), (10,12), (3,4), (3,0), (8,4), (1,10), (2,14), (8,10), (9,0),]
folds = [(1,7), (0,5),]

for j,(d,f) in enumerate(folds):
	for i,p in enumerate(points):
		if p[d] > f:
			p = list(p)
			p[d] = f - (p[d] - f)
			points[i] = tuple(p)
	if j == 0:
		print(len(set(points)))

points = set(points)
xmin = min(p[0] for p in points)
xmax = max(p[0] for p in points)
ymin = min(p[1] for p in points)
ymax = max(p[1] for p in points)
for y in range(ymin,ymax+1):
	for x in range(xmin,xmax+1):
		if (x,y) in points:
			print('#',end='')
		else:
			print(' ',end='')
	print()
