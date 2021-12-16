dat = """...snip...""".split("\n")
dat = [[int(j) for j in i] for i in dat]
CY = len(dat)
CX = len(dat[0])
def n(y,x):
	if y > 0:
		yield y-1,x
	if y < CY-1:
		yield y+1,x
	if x > 0:
		yield y,x-1
	if x < CX-1:
		yield y,x+1
s=0
l = []
for y in range(CY):
	for x in range(CX):
		if all(dat[y][x] < dat[ny][nx] for ny,nx in n(y,x)) and dat[y][x] < 9:
			s+=dat[y][x]+1
			l.append((y,x))
print(s)
bs = []
for by,bx in l:
	seen = set()
	todo = [(by,bx)]
	while todo:
		y,x = todo.pop()
		if (y,x) in seen:
			continue
		seen.add((y,x))
		for ny,nx in n(y,x):
			if dat[ny][nx] > dat[y][x] and dat[ny][nx] < 9:
				todo.append((ny,nx))
	bs.append(len(seen))
bs.sort()
print(bs[-1] * bs[-2] * bs[-3])
