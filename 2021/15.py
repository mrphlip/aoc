dat="""1964778752979887222739789777935919929996793679617497991954953881381939846468999159686925929898196249
[...]"""
#dat = """1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"""
dat = [[int(c) for c in i] for i in dat.split("\n")]

CY = len(dat)
CX = len(dat[0])
N = 5
totrisk = [[None for x in range(CX*N)] for y in range(CY*N)]
totrisk[0][0] = 0
candidates = {(0,0)}
done = set()
ndone = 1
while (CX*N-1,CY*N-1) not in done:
	minval = minat = None
	for y,x in candidates:
		if minval is None or totrisk[y][x] < minval:
			minval = totrisk[y][x]
			minat = y,x
	y,x = minat
	candidates.remove((y,x))
	done.add((y,x))
	for dx,dy in [(-1,0),(1,0),(0,-1),(0,1)]:
		nx = x + dx
		ny = y + dy
		if 0 <= nx < CX*N and 0 <= ny < CY*N:
			val = totrisk[y][x] + (dat[ny%CY][nx%CX] + (ny//CY) + (nx//CX) - 1) % 9 + 1
			if totrisk[ny][nx] is None or totrisk[ny][nx] > val:
				totrisk[ny][nx] = val
				candidates.add((ny,nx))
print(totrisk[-1][-1])
