#!/usr/bin/python
from aocimports import *

dat = """\
563  334 22 38 2624 [...]"""
dat2 = """\
*   +    *  +  +    *   [...]"""

#dat = "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314"; dat2 = "*   +   *   +  "

cols = {i for i, c in enumerate(dat2) if c != " "}
ops = [c for i, c in enumerate(dat2) if c != " "]
vals = [[int(i) for i in row.split()] for row in dat.split("\n")]
assert all(len(row) == len(ops) for row in vals)

res = [sum(row[ix] for row in vals) if op == "+" else prod(row[ix] for row in vals) for ix, op in enumerate(ops)]
print(sum(res))

dat = dat.split("\n")
curr = None
vals = []
for ix in range(len(dat2)):
	if ix in cols:
		curr = []
		vals.append(curr)
	val = ''.join(row[ix] for row in dat).strip()
	if val:
		curr.append(int(val))
if not curr:
	del vals[-1]
assert len(vals) == len(ops)

res = [sum(vals[ix]) if op == "+" else prod(vals[ix]) for ix, op in enumerate(ops)]
print(sum(res))

