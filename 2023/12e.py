#!/usr/bin/python -u
dat = [
	("??????#??#??", (1,1,5,1)),
	("?#?#??##?#?", (2,5,1)),
	[...snip...]
]

#dat = [("???.###",(1,1,3)),(".??..??...?##.",(1,1,3)),("?#?#?#?#?#?#?#?",(1,3,1,6)),("????.#...#...",(4,1,1)),("????.######..#####.",(1,6,5)),("?###????????",(3,2,1)),]

def possibilities4(val, criteria):
	def worker(valix, criteriaix, dotsleft, hashesleft):
		if criteriaix >= len(criteria):
			if any(i == '#' for i in val[valix:]):
				#print("=>0")
				return 0
			else:
				#print("=>1")
				return 1

		while valix < len(val) and val[valix] == '.':
			valix += 1
		if valix >= len(val):
			#print("bad end")
			return 0

		res = 0
		if val[valix] == '?' and dotsleft:
			res += memoworker(valix + 1, criteriaix, dotsleft - 1, hashesleft)

		if valix + criteria[criteriaix] <= len(val) and not any(i == '.' for i in val[valix:valix + criteria[criteriaix]]):
			if (valix + criteria[criteriaix] == len(val) or val[valix + criteria[criteriaix]] != '#'):
				rmhash = val[valix:valix + criteria[criteriaix]].count('?')
				rmdot = (valix + criteria[criteriaix] < len(val) and val[valix + criteria[criteriaix]] == '?')
				if rmhash <= hashesleft and rmdot <= dotsleft:
					res += memoworker(valix + criteria[criteriaix] + 1, criteriaix + 1, dotsleft - rmdot, hashesleft - rmhash)

		return res

	memo = {}
	def memoworker(valix, criteriaix, dotsleft, hashesleft):
		k = (valix, criteriaix, dotsleft, hashesleft)
		if k in memo:
			return memo[k]
		else:
			res = worker(valix, criteriaix, dotsleft, hashesleft)
			memo[k] = res
			return res

	numhashes = sum(criteria)
	numdots = len(val) - sum(criteria)
	existhashes = val.count('#')
	existdots = val.count('.')
	return worker(0, 0, numdots - existdots, numhashes - existhashes)

print(sum(possibilities4(a,b) for a,b in dat))
print(sum(possibilities4("?".join([a]*5),b*5) for a,b in dat))
