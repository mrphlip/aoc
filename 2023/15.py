#!/usr/bin/python
from collections import OrderedDict

dat = "vpq=6,fbf-,pft-,[...snip...]".split(",")

#dat = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7".split(",")

def gethash(s):
	n = 0
	for i in s:
		n += ord(i)
		n = (n*17)%256
	return n
print(sum(gethash(i) for i in dat))

boxes=[OrderedDict() for i in range(256)]
for i in dat:
	if '-' in i:
		l, r = i.split('-')
		box = gethash(l)
		boxes[box].pop(l, None)
	else:
		l, r = i.split('=')
		box = gethash(l)
		r = int(r)
		boxes[box][l] = r
#print([(i,v) for i,v in enumerate(boxes) if v])
print(sum(ix*slot*r for ix,box in enumerate(boxes,1) for slot, r in enumerate(box.values(), 1)))
