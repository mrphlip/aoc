#!/usr/bin/python3
a = """ZNNvFWHqLNPZHHqPTHHnTGBhrrpjvmwfMmpfpjBjwpmw
sbdzQgzgssgbglRtmjlwhjBlfrSrMt
[...snip...]""".split("\n")
b = []
for x in a:
	l = set(x[:len(x)//2])
	r = set(x[len(x)//2:])
	i, = l & r
	i = ord(i)
	if i > 0x60:
		i -= 0x60
	else:
		i -= 0x40
		i += 26
	b.append(i)
print(sum(b))

b=[]
for x in range(0, len(a), 3):
	l = set(a[x])
	m = set(a[x+1])
	r = set(a[x+2])
	i, = l & m & r
	i = ord(i)
	if i > 0x60:
		i -= 0x60
	else:
		i -= 0x40
		i += 26
	b.append(i)
print(sum(b))
