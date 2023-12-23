#!/usr/bin/python -u
dat = [
	("??????#??#??", (1,1,5,1)),
	("?#?#??##?#?", (2,5,1)),
	[...snip...]
]

#dat = [("???.###",(1,1,3)),(".??..??...?##.",(1,1,3)),("?#?#?#?#?#?#?#?",(1,3,1,6)),("????.#...#...",(4,1,1)),("????.######..#####.",(1,6,5)),("?###????????",(3,2,1)),]

def gen(val):
	return tuple(len(i) for i in val.split(".") if i)

def possibilities(val):
	if '?' in val:
		ix = val.index('?')
		yield from possibilities(val[:ix] + '.' + val[ix+1:])
		yield from possibilities(val[:ix] + '#' + val[ix+1:])
	else:
		yield val

def count(val, criteria):
	return sum(1 for i in possibilities(val) if gen(i) == criteria)

print(sum(count(a,b) for a,b in dat))
