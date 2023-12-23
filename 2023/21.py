#!/usr/bin/python
dat = """\
...................................................................................................................................
...##......#....#...#.....#.#....#...#...#...#......#........................##......#..#..#.#........#....................##......
...#.......#..........#..#....#................#...##.....##...................#..........###......#.....#.........#....#.#........
[...snip...]""".split("\n")

#dat = "...........\n.....###.#.\n.###.##..#.\n..#.#...#..\n....#.#....\n.##..S####.\n.##..#...#.\n.......##..\n.##.#.####.\n.##..##.##.\n...........".split("\n")

#dat = """S......\n.#.##.#\n.#.##.#\n.#.##.#\n.#.#..#\n.#.#..#\n.#.##.#""".split("\n")
#dat = """S......\n.#..#.#\n.#.#..#\n.#..#.#\n.#.#..#\n.#..#.#\n.#.#..#""".split("\n")

#dat = ".....\n.....\n..S..\n.....\n.....".split("\n")

CY = len(dat)
CX = len(dat[0])

startx = starty = None
for y, row in enumerate(dat):
	if 'S' in row:
		startx = row.index('S')
		starty = y
		dat[y] = row[:startx] + '.' + row[startx+1:]
		break

dists = {}
todo = [(startx,starty,0)]
while todo:
	x, y, d = todo.pop(0)
	if (x,y) in dists:
		continue
	if not (0 <= x < CX and 0 <= y < CY):
		continue
	if dat[y][x] == '#':
		continue
	dists[x,y] = d
	for dx, dy in [(0,-1),(0,1),(1,0),(-1,0)]:
		todo.append((x+dx,y+dy,d+1))
print(len([1 for d in dists.values() if d <= 64 and d%2 == 0]))

dat = dat[starty:] + dat[:starty]
dat.append("." * CX)
dat = [i[startx:] + i[:startx] + "." for i in dat]
#print("\n".join(dat))

def solvefrom(startx, starty):
	dists = {}
	todo = [(startx,starty,0)]
	while todo:
		x, y, d = todo.pop(0)
		if (x,y) in dists:
			continue
		if not (0 <= x <= CX and 0 <= y <= CY):
			continue
		if dat[y][x] == '#':
			continue
		dists[x,y] = d
		for dx, dy in [(0,-1),(0,1),(1,0),(-1,0)]:
			todo.append((x+dx,y+dy,d+1))
	for x,y in list(dists.keys()):
		if x >= CX or y >= CY:
			del dists[x,y]
	return dists

fromtopleft = solvefrom(0,0)
fromtopright = solvefrom(CX,0)
frombottomleft = solvefrom(0,CY)
frombottomright = solvefrom(CX,CY)
maxdist = max(max(fromtopleft.values()), max(fromtopright.values()), max(frombottomleft.values()), max(frombottomright.values()))

print(maxdist)

N = 26501365
#N = maxdist + 2*CX
#N = 300
#N = 100
#N = 551
print(N)
Nparity = N%2

#      F
#     AEF
#    ABDEF
#   ABC#DEF
#  ABC###DEF
# GHI#####JKL
#  GHI###JKL
#   GHI#JKL
#    GHJKL
#     GKL
#      L

r = (N - maxdist) // CX
print(r)
def count(dists, ofs):
	ofs *= CX
	return len([1 for d in dists.values() if (d+ofs) <= N and (d+ofs)%2 == Nparity])
A_ =count(frombottomright, r+2) 
A = count(frombottomright, r+1)
B = count(frombottomright, r)
C = count(frombottomright, r-1)
D = count(frombottomleft, r)
E = count(frombottomleft, r+1)
F = count(frombottomleft, r+2)
G = count(fromtopright, r+2)
H = count(fromtopright, r+1)
I = count(fromtopright, r)
J = count(fromtopleft, r+1)
K = count(fromtopleft, r+2)
L = count(fromtopleft, r+3)

def show(dists):
	for y in range(CY):
		for x in range(CX):
			if (x,y) in dists:
				print("%3d"%dists[x,y], end=' ')
			else:
				print("###", end=' ')
		print()
	print()
#show(fromtopleft)
#show(fromtopright)
#show(frombottomleft)
#show(frombottomright)

#print(count(frombottomright, r+3))
#print(count(frombottomright, r-2))
#print(count(frombottomleft, r+3))
#print(count(frombottomleft, r-1))
#print(count(fromtopright, r+3))
#print(count(fromtopright, r-1))
#print(count(fromtopleft, r+4))
#print(count(fromtopleft, r))

even = len([1 for d in fromtopleft.values() if d%2 == Nparity])
odd = len([1 for d in fromtopleft.values() if d%2 == 1-Nparity])

# numeven = 1 + 8 + 16 + ... + r*4 = 1 + 8(1+2+...+r/2) = 1 + 4(r/2)(r/2+1)
# numodd = 4 + 12 + ... + (r-1)*4 = 4(1+3+...+(r-1)) = 4*(r/2)^2
# NB: r is even
numeven = 1 + 4*(r//2)*(r//2+1)
numodd = 4*(r//2)*(r//2)
numA_ = r+3
numA = r+2
numB = r+1
numC = r
numD = r+1
numE = r+2
numF = r+3
numG = r+3
numH = r+2
numI = r+1
numJ = r+2
numK = r+3
numL = r+4

print(A_,A,B,C,D,E,F,G,H,I,J,K,L,even,odd)
print(numA_,numA,numB,numC,numD,numE,numF,numG,numH,numI,numJ,numK,numL,numeven,numodd)
print(sum([numA,numB,numC,numD,numE,numF,numG,numH,numI,numJ,numK,numL,numeven,numodd]))
print((r+4)**2+(r+3)**2)

print(numeven, numodd)
print(numeven + numodd)
print((r+1)**2+r**2)

total = even * numeven + odd * numodd + A_*numA_ + A*numA + B*numB + C*numC + D*numD + E*numE + F*numF + G*numG + H*numH + I*numI + J*numJ + K*numK + L*numL
print(total)
