#!/usr/bin/python
import re

dat = [
	"twovgtprdzcjjzkq3ffsbcblnpq",
	"two8sixbmrmqzrrb1seven",
	"9964pfxmmr474",
	[...snip...]
]

calibs = []
for i in dat:
	i = [c for c in i if c in '0123456789']
	calibs.append(int(i[0] + i[-1]))
print(sum(calibs))

#dat = ["two1nine","eightwothree","abcone2threexyz","xtwone3four","4nineeightseven2","zoneight234","7pqrstsixteen"]

lookup = {j:str(i) for i,j in enumerate(["one","two","three","four","five","six","seven","eight","nine"], 1)}
for i in "0123456789":
	lookup[i] = i
re_num = re.compile("(%s)" % "|".join(lookup.keys()))
re_rev = re.compile("(%s)" % "|".join(i[::-1] for i in lookup.keys()))

calibs = []
for i in dat:
	first = re_num.search(i).group(1)
	first = lookup[first]
	last = re_rev.search(i[::-1]).group(1)
	last = lookup[last[::-1]]
	calibs.append(int(first + last))
#print(calibs)
print(sum(calibs))

