#!/usr/bin/python3
dat = [
	(2483411, 3902983, 2289579, 3633785),(3429446, 303715, 2876111, -261280),[...snip...]
]
#dat = [(2, 18, -2, 15),(9, 16, 10, 16),(13, 2, 15, 3),(12, 14, 10, 16),(10, 20, 10, 16),(14, 17, 10, 16),(8, 7, 2, 10),(2, 0, 2, 10),(0, 11, 2, 10),(20, 14, 25, 17),(17, 20, 21, 22),(16, 7, 15, 3),(14, 3, 15, 3),(20, 1, 15, 3)]

def find_at(ty, minx=None, maxx=None):
	ranges = []
	for sx, sy, bx, by in dat:
		dx = abs(sx - bx)
		dy = abs(sy - by)
		dist = dx + dy
		remaining_dist = dist - abs(ty - sy)
		if remaining_dist >= 0:
			ranges.append((sx - remaining_dist, sx + remaining_dist + 1))
	ranges.sort()
	lastb = ranges[0][0] - 1
	flatranges = []
	for a, b in ranges:
		if minx is not None:
			a = max(a, minx)
			b = max(b, minx)
			a = min(a, maxx)
			b = min(b, maxx)
		if a >= b:
			continue
		if lastb >= b:
			continue
		if a <= lastb:
			flatranges[-1][1] = b
		else:
			flatranges.append([a, b])
		lastb = b
	return sum(b-a for a,b in flatranges), flatranges

targety = 2000000
#targety = 10
t, _ = find_at(targety)
t -= len(set((bx,by) for _,_,bx,by in dat if by == targety))
print(t)

limit = 4000000
#limit = 20
for y in range(limit + 1):
	#if y % 1000 == 0:
	#	print(f"[{y}]")
	t, ranges = find_at(y, 0, limit + 1)
	if t != limit + 1:
		break
x = ranges[0][1]
print(x * 4000000 + y)
