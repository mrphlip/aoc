#!/usr/bin/python3
dat = [
	("NQ",0,["SU", "XD"]),("AB",0,["XD", "TE"]),("IA",0,["CS", "WF"]),("WD",0,["DW", "II"]),("XD",10,["AB", "NQ", "VT", "SC", "MU"]),("SL",0,["RP", "DS"]),("FQ",15,["EI", "YC"]),("KF",0,["FL", "QP"]),("QP",0,["KF", "RP"]),("DS",0,["SL", "AA"]),("IK",0,["XC", "AA"]),("HQ",0,["VM", "WV"]),("WR",0,["WV", "HF"]),("HH",20,["PI", "CF", "CN", "NF", "AR"]),("DW",19,["KD", "WD", "HS"]),("RP",14,["SL", "QP", "BH", "LI", "WP"]),("EC",0,["NF", "XC"]),("AA",0,["NH", "ES", "UC", "IK", "DS"]),("VM",18,["HQ"]),("NF",0,["HH", "EC"]),("PS",0,["AR", "SU"]),("IL",0,["XC", "KZ"]),("WP",0,["CS", "RP"]),("WF",0,["FL", "IA"]),("XW",0,["OL", "NL"]),("EH",0,["UK", "YR"]),("UC",0,["AA", "FL"]),("CS",3,["IA", "CN", "LD", "RJ", "WP"]),("AR",0,["PS", "HH"]),("CF",0,["HH", "FL"]),("NH",0,["AA", "LD"]),("RJ",0,["DJ", "CS"]),("XC",17,["IL", "EC", "YR", "IK", "DJ"]),("TE",24,["AB", "YA"]),("CN",0,["HH", "CS"]),("KD",0,["DW", "UK"]),("SC",0,["EI", "XD"]),("MU",0,["XD", "YP"]),("SU",22,["PS", "LI", "II", "NQ"]),("FL",8,["KF", "WF", "CF", "UC", "HS"]),("OL",4,["KZ", "HF", "XW"]),("EI",0,["FQ", "SC"]),("NL",0,["XW", "UK"]),("YP",21,["YA", "MU", "YC"]),("BH",0,["VT", "RP"]),("II",0,["SU", "WD"]),("YA",0,["TE", "YP"]),("HS",0,["FL", "DW"]),("DJ",0,["RJ", "XC"]),("KZ",0,["OL", "IL"]),("YR",0,["EH", "XC"]),("UK",7,["KD", "NL", "EH"]),("YC",0,["FQ", "YP"]),("ES",0,["PI", "AA"]),("LI",0,["SU", "RP"]),("LD",0,["NH", "CS"]),("VT",0,["BH", "XD"]),("PI",0,["ES", "HH"]),("WV",11,["WR", "HQ"]),("HF",0,["OL", "WR"])
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
