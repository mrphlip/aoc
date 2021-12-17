from math import sqrt,floor,ceil
xmin,xmax=(14,50)
ymin,ymax=(-267,-225)
#xmin,xmax,ymin,ymax=20,30,-10,-5

def project(vx,vy):
	x,y = 0,0
	while True:
		x += vx
		y += vy
		if vx > 0:
			vx -= 1
		elif vx < 0:
			vx += 1
		vy -= 1
		#print(x,y,vx,vy)
		if xmin <= x <= xmax and ymin <= y <= ymax:
			return 0
		elif x > xmax:
			return 1
		elif y < ymin:
			return -1

max_vy = abs(ymin) - 1
print(max_vy * (max_vy+1) // 2)

count = 0
for vx in range(0,xmax+1):
	for vy in range(ymin, max_vy+1):
		if project(vx,vy) == 0:
			count += 1
print(count)
