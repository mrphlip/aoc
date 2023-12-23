#!/usr/bin/python3
dat = [
	"21211-122",
	"1=--02-10=-=00=-0",
	"1=-2",
	[...snip...]
]

digits = {'0':0, '1':1, '2':2, '-':-1, '=':-2}
digits_rev = {v:k for k,v in digits.items()}
def from_num(s):
	d = 0
	for c in s:
		d = d * 5 + digits[c]
	return d
def to_num(n):
	if not n:
		return '0'
	s = ''
	while n:
		n, d = divmod(n, 5)
		if d >= 3:
			d -= 5
			n += 1
		s = digits_rev[d] + s
	return s
print(to_num(sum(from_num(i) for i in dat)))
