#!/usr/bin/python3
a = """    M V L
G   VCG D
[...snip...]""".split("\n")
a = [[j[i] for j in a if j[i] != ' '] for i in range(9)]
dat = [
	(1,5,2),(7,7,1),[...snip...]
]
for n, f, t in dat:
	m = a[f-1][:n]
	a[f-1][:n] = []
	a[t-1][:0] = m[::-1]  # remove the [::-1] for part B
for i in a:
	print(i[0], end='')
print()
