#!/usr/bin/python3
a = [
	(37,87,36,87),(3,98,3,84),[...snip...]
]
#a = [(2,4,6,8),(2,3,4,5),(5,7,7,9),(2,8,3,7),(6,6,4,6),(2,6,4,8)]
t = 0
for w,x,y,z in a:
	if (w <= y and z <= x) or (y <= w and x <= z):
		t += 1
print(t)
t = 0
for w,x,y,z in a:
	if w <= z and x >= y:
		t += 1
print(t)
