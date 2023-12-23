#!/usr/bin/python -u
dat = [
	("??????#??#??", (1,1,5,1)),
	("?#?#??##?#?", (2,5,1)),
	[...snip...]
]

#dat = [("???.###",(1,1,3)),(".??..??...?##.",(1,1,3)),("?#?#?#?#?#?#?#?",(1,3,1,6)),("????.#...#...",(4,1,1)),("????.######..#####.",(1,6,5)),("?###????????",(3,2,1)),]

def blockAt(val, blockpos, blocklen):
	if blockpos < 0 or blockpos + blocklen > len(val):
		return False
	if blockpos > 0 and val[blockpos-1] == '#':
		return False
	if blockpos + blocklen < len(val) and val[blockpos + blocklen] == '#':
		return False
	if any(val[i] == '.' for i in range(blockpos, blockpos + blocklen)):
		return False
	return True

def possibilities3(val, criteria):
	# find the earliest each block can be, if we squish them up against each end
	blockstarts = []
	ix = 0
	for i in criteria:
		while not blockAt(val, ix, i):
			ix += 1
			if i + ix > len(val):
				return
		blockstarts.append(ix)
		ix += i + 1
	# find the latest each block can be
	blockends = []
	ix = len(val) + 1
	for i in criteria[::-1]:
		ix -= i + 1
		while not blockAt(val, ix, i):
			ix -= 1
			if ix < 0:
				return
		blockends.append(ix)
	blockends = blockends[::-1]

	# find out the latest each block can be, if we stretch them out away from the ends
	blockstarts2 = []
	i = 0
	ix = 0
	while ix < len(criteria):
		while i < len(val) - 1 and val[i] != '#':
			i += 1
		j = i
		while j < len(val) - 1 and val[j] == '#':
			j += 1
		blocklen = j - i
		blockstarts2.append(i)
		while ix < len(criteria) and criteria[ix] < blocklen:
			ix += 1
			blockstarts2.append(i)
		i = min(i + criteria[ix], len(val) - 1)
		ix += 1

	blockends2 = []
	i = len(val) - 1
	ix = len(criteria) - 1
	while ix >= 0:
		while i > 0 and val[i] != '#':
			i -= 1
		j = i
		while j > 0 and val[j] == '#':
			j -= 1
		blocklen = i - j
		blockends2.append(i - criteria[ix] + 1)
		while ix >= 0 and criteria[ix] < blocklen:
			ix -= 1
			blockends2.append(i - criteria[ix] + 1)
		i = max(i - criteria[ix], 0)
		ix -= 1
	blockends2 = blockends2[::-1]

	#print(blockstarts, blockends, blockstarts2, blockends2)
	blockstarts = [max(a,b) for a,b in zip(blockstarts, blockends2)]
	blockends = [min(a,b) for a,b in zip(blockstarts2, blockends)]
	#print(blockstarts, blockends)

	def worker(ix, x, blocks):
		if ix >= len(criteria):
			#print("considering set")
			if not any(i == '#' for i in val[blocks[-1][1]:]):
				#print("yes", blocks)
				yield blocks
			return
		if blocks:
			startat = max(blockstarts[ix], blocks[-1][1] + 1)
			if any(i == '#' for i in val[blocks[-1][1]:startat]):
				return
		else:
			startat = blockstarts[0]
		for i in range(startat, blockends[ix] + 1):
			#print("considering",ix,i)
			if i > 0 and val[i-1] == '#':
				return
			if blockAt(val, i, criteria[ix]):
				blocks.append((i, i+criteria[ix]))
				yield from worker(ix + 1, i - blockstarts[ix], blocks)
				blocks.pop(-1)
	return sum(1 for i in worker(0, 0, []))

print(sum(possibilities3(a,b) for a,b in dat))
print(sum(possibilities3("?".join([a]*5),b*5) for a,b in dat))
