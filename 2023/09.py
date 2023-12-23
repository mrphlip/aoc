#!/usr/bin/python
dat = [
	(4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44),
	(8,10,21,62,176,448,1042,2259,4634,9116,17421,32738,61151,114506,216170,412530,795822,1549262,3035999,5973950,11778148),
	(16,30,62,134,281,559,1056,1909,3332,5662,9448,15671,26340,46040,85632,170374,356396,761786,1621354,3376772,6814703),
	[...snip...]
]

#dat = [(0,3,6,9,12,15), (1,3,6,10,15,21), (10,13,16,21,30,45), ]

def extrap(n):
	a = [n]
	while not all(i == 0 for i in n):
		n = [b-a for a,b in zip(n[:-1],n[1:])]
		a.append(n)
	return sum(i[-1] for i in a)
print(sum(extrap(i) for i in dat))

def extrap2(n):
	a = [n]
	while not all(i == 0 for i in n):
		n = [b-a for a,b in zip(n[:-1],n[1:])]
		a.append(n)
	x = 0
	for i in reversed(a):
		x = i[0] - x
	return x
#print([extrap2(i) for i in dat])
print(sum(extrap2(i) for i in dat))
