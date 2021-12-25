dat = """.>.>.......v.v.....vvv>..v...>>>.>.v.>.v....>.v.vv.>..>.>>>.....v...>..>v..v.>>.>v>vv.>v.v>v>.>>>.>>>vvv.v>.v>>>.........vv.v.v>>.v>v>v..v.
vv.....vvv.>.vv.vv.v..v..v>.>..v..v>>>..v.v.v>.>vvvv.>....v...>>.>..>>v.v>>v>>.>..v..>>>.>>.>.>.>v...vv.vv.>.>>>..v..>v>v>>...>v...v>>v>..v
[...]""".split("\n")
CY = len(dat)
CX = len(dat[0])

cukes = {}
for y,row in enumerate(dat):
	for x,c in enumerate(row):
		if c == '>':
			cukes[x,y] = True
		elif c == 'v':
			cukes[x,y] = False

def step(x,y,dir):
	if dir:
		return (x+1)%CX,y
	else:
		return x,(y+1)%CY
def dostep(dir):
	tomove = []
	for x,y in cukes:
		if cukes[x,y] == dir:
			x2,y2 = step(x,y,dir)
			if (x2,y2) not in cukes:
				tomove.append((x,y,x2,y2))
	for x,y,x2,y2 in tomove:
		del cukes[x,y]
		cukes[x2,y2] = dir
	return len(tomove)

a = 1
i = 0
while a:
	a = dostep(True)
	a += dostep(False)
	i += 1
print(i)
