#!/usr/bin/python3
from functools import cmp_to_key
dat = [
	([[1,[0,3,5,[2,1,3,3,5]],4,[[],5]],[],[0,[7,[5],7,7]]],[[[],[[],[5,2,8,9,7],1,5],[3,[]]]]),([[[[1,3,6],[7,9,2,7],[5,0,5,8,4]]],[[8,2,6,[]],[],1,6]],[[5,10,4],[6,9,8],[5,[4,[3,8,1,5,1],[4],6,[3]],[[0]],[10],[[5,7],[5,1,1,4,7],[]]],[[[9,7,5,7],[5,1,7,3,1],[8,4,2]],5,[[8,9,3,4,4],4,[2,4,2],3,[6,10,5,7]],9],[[1]]]),[...snip...]
]
def compare(a,b):
	if isinstance(a, int) and isinstance(b, int):
		return a - b
	elif isinstance(a, list) and isinstance(b, list):
		for x, y in zip(a, b):
			ret = compare(x, y)
			if ret != 0:
				return ret
		return len(a) - len(b)
	elif isinstance(a, list):
		return compare(a, [b])
	elif isinstance(b, list):
		return compare([a], b)

print(sum(i for i, (a, b) in enumerate(dat, 1) if compare(a, b) < 0))

packets = [ [[2]], [[6]] ]
for a, b in dat:
	packets.append(a)
	packets.append(b)
packets.sort(key=cmp_to_key(compare))
a = packets.index([[2]]) + 1
b = packets.index([[6]]) + 1
print(a * b)
