#!/usr/bin/python -u
dat = [
	("??????#??#??", (1,1,5,1)),
	("?#?#??##?#?", (2,5,1)),
	[...snip...]
]

#dat = [("???.###",(1,1,3)),(".??..??...?##.",(1,1,3)),("?#?#?#?#?#?#?#?",(1,3,1,6)),("????.#...#...",(4,1,1)),("????.######..#####.",(1,6,5)),("?###????????",(3,2,1)),]

def possibilities2(val, criteria, startix=0, startcrit=0):
	if startcrit >= len(criteria):
		if startix >= len(val) or all(i in '.?' for i in val[startix:]):
			yield 1
		return

	for i in range(startix, len(val) - criteria[startcrit] + 1):
		if all(i in '#?' for i in val[i:i+criteria[startcrit]]) and (i+criteria[startcrit] >= len(val) or val[i+criteria[startcrit]] in '.?'):
			yield from possibilities2(val, criteria, i+criteria[startcrit]+1, startcrit+1)
		if val[i] == '#':
			break

def count2(val, criteria):
	return sum(possibilities2(val, criteria))

print(sum(count2(a,b) for a,b in dat))
print(sum(count2("?".join([a]*5),b*5) for a,b in dat))
