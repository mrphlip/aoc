#!/usr/bin/python3
class A:
	def __init__(self, val):
		self.val = val

dat = [
	9038,7675,-2761,[...snip...]
]
#dat = [1, 2, -3, 3, -2, 0, 4]
dat = [i * 811589153 for i in dat] # remove for part 1
dat = [A(i) for i in dat]

seq = dat[:]
zero, = [i for i in dat if i.val == 0]

def mix():
	for i in dat:
		ix = seq.index(i)
		newix = (ix + i.val) % (len(dat) - 1)
		del seq[ix]
		seq.insert(newix, i)

def grove():
	ix = seq.index(zero)
	a = seq[(ix + 1000) % len(seq)].val
	b = seq[(ix + 2000) % len(seq)].val
	c = seq[(ix + 3000) % len(seq)].val
	return a + b + c

for i in range(10):  # remove loop for part 1
	mix()
print(grove())
