dat = """...snipped 100 lines...""".split("\n")
mapping = {'[':']','(':')','{':'}','<':'>'}
points = {']':57,')':3,'}':1197,'>':25137}
points2 = {c:i for i,c in enumerate('([{<',1)}
def check(s):
	stack = []
	for i,c in enumerate(s):
		if c in '([{<':
			stack.append(c)
		else:
			a = mapping[stack.pop()]
			if a != c:
				return points[c], 0
	score = 0
	for c in stack[::-1]:
		score = score * 5 + points2[c]
	return 0, score
print(sum(check(i)[0] for i in dat))
a = [res[1] for i in dat for res in [check(i)] if res[0] == 0]
a.sort()
#print(len(a))
print(a[len(a)//2])
