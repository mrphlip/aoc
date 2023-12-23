#!/usr/bin/python -u
dat = [
	("??????#??#??", (1,1,5,1)),
	("?#?#??##?#?", (2,5,1)),
	[...snip...]
]

#dat = [("???.###",(1,1,3)),(".??..??...?##.",(1,1,3)),("?#?#?#?#?#?#?#?",(1,3,1,6)),("????.#...#...",(4,1,1)),("????.######..#####.",(1,6,5)),("?###????????",(3,2,1)),]

def possibilities4(v, c):
	def worker(val, criteria, dotsleft, hashesleft):
		#print(val, criteria, dotsleft)
		if not criteria:
			if any(i == '#' for i in val):
				#print("=>0")
				return 0
			else:
				#print("=>1")
				return 1

		val = val.lstrip('.')
		if not val:
			#print("bad end")
			return 0
		if val[0] == '#':
			if len(val) >= criteria[0] and not any(i == '.' for i in val[:criteria[0]]):
				if (criteria[0] == len(val) or val[criteria[0]] != '#'):
					rmhash = val[:criteria[0]].count('?')
					rmdot = (criteria[0] < len(val) and val[criteria[0]] == '?')
					if rmhash <= hashesleft and rmdot <= dotsleft:
						return worker(val[criteria[0]+1:], criteria[1:], dotsleft - rmdot, hashesleft - rmhash)
			#print("bad end 2")
			return 0
		else:
			assert val[0] == '?'
			res = 0
			if hashesleft:
				res = worker('#' + val[1:], criteria, dotsleft, hashesleft - 1)
			if dotsleft:
				res += worker(val[1:], criteria, dotsleft - 1, hashesleft)
			#print("==>", res)
			return res
	numhashes = sum(c)
	numdots = len(v) - sum(c)
	existhashes = v.count('#')
	existdots = v.count('.')
	return worker(v, c, numdots - existdots, numhashes - existhashes)

print(sum(possibilities4(a,b) for a,b in dat))
print(sum(possibilities4("?".join([a]*5),b*5) for a,b in dat))
