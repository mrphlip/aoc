dat = [
[3,[5,[7,[3,9]]]],
[[[[7,0],0],[2,[2,8]]],[[[7,8],1],3]],
[...]
]

from copy import deepcopy
def add(a,b):
	return reduce([a,b])
def reduce(x):
	x = deepcopy(x)
	cont = True
	while cont:
		cont = False
		stack = [(0,x,[])]
		while stack:
			n,v,p = stack.pop()
			if not isinstance(v, list):
				continue
			if n >= 4:
				for a, ix in p:
					if ix > 0:
						while isinstance(a[ix-1], list):
							a = a[ix-1]
							ix = len(a)
						a[ix-1] += v[0]
						break
				for a, ix in p:
					if ix < len(a)-1:
						while isinstance(a[ix+1], list):
							a = a[ix+1]
							ix = -1
						a[ix+1] += v[1]
						break
				p[0][0][p[0][1]] = 0
				cont = True
				break
			else:
				for i,sub in list(enumerate(v))[::-1]:
					stack.append((n+1,sub,[[v,i]]+p))
		if cont:			
			continue

		stack = [(0,x,None,None)]
		while stack:
			n,v,p,ix = stack.pop()
			if not isinstance(v, list):
				if v >= 10:
					p[ix] = [v // 2, (v+1) // 2]
					cont = True
					break
			else:
				for i,sub in list(enumerate(v))[::-1]:
					stack.append((n+1,sub,v,i))
	return x

def magnitude(x):
	if isinstance(x, list):
		return 3*magnitude(x[0]) + 2*magnitude(x[1])
	else:
		return x

sum = dat[0]
for i in dat[1:]:
	sum = add(sum, i)
print(magnitude(sum))

maxval = -1
for i in dat:
	for j in dat:
		val = magnitude(add(i,j))
		maxval = max(maxval, val)
print(maxval)
