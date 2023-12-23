#!/usr/bin/python3

a = [
	("C","Y"),("B","Y"),[...snip...]
]
#a = [('A','Y'),('B','X'),('C','Z')]
s = 0
for l, r in a:
	l = [0,1,2][ord(l) - ord('A')]
	r = [0,1,2][ord(r) - ord('X')]
	#r = (l + r - 1) % 3  # (uncomment this line for part 2)
	res = (r - l) % 3
	s += 1 + r + [3,6,0][res]
	#print(s)
print(s)
