#!/usr/bin/python3
dat = [
	("NQ",0,["SU", "XD"]),("AB",0,["XD", "TE"]),[...snip...]
]
#dat = [("AA",0,["DD", "II", "BB"]),("BB",13,["CC", "AA"]),("CC",2,["DD", "BB"]),("DD",20,["CC", "AA", "EE"]),("EE",3,["FF", "DD"]),("FF",0,["EE", "GG"]),("GG",0,["FF", "HH"]),("HH",22,["GG"]),("II",0,["AA", "JJ"]),("JJ",21,["II"])]
valves = [a for a, b, c in dat]
flows = {a: b for a, b, c in dat}
neighbours = {a: c for a, b, c in dat}

distmap = {}
for f in valves:
	d = {k:None for k in valves}
	d[f] = 0
	while True:
		c = [(d[k]+1,n) for k in valves if d[k] is not None for n in neighbours[k] if d[n] is None]
		if not c:
			break
		c.sort()
		c = [i for i in c if i[0] == c[0][0]]
		for n, k in c:
			d[k] = n
	for k, n in d.items():
		distmap[f, k] = n

def consider(node, range, score, enabled):
	#foundany = False
	for k in valves:
		if k != node and flows[k] > 0 and k not in enabled and distmap[node, k] <= range:
			yield from consider(k, range-distmap[node, k]-1, score+flows[k]*(range-distmap[node,k]-1), enabled+[k])
			#foundany = True

	#if not foundany:
	yield score, enabled

res = [(n, set(e)) for n, e in consider("AA", 26, 0, [])]
#print(max(n1 + n2 for n1, e1 in res for n2, e2 in res if not (e1 & e2)))
def gen():
	for i, (n1, e1) in enumerate(res):
		if i % 100 == 0:
			print(' ',i,len(res))
		for n2, e2 in res:
			if not e1 & e2:
				yield n1 + n2
print(max(gen()))
