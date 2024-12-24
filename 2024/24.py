#!/usr/bin/python
from aocimports import *

dat = {
	"x00": 1,
	"x01": 0,
	[...]
}

dat2 = [
	("x00", "AND", "y00", "wrs"),
	("y35", "XOR", "x35", "cbq"),
	[...]
]

#dat = {"x00": 1,"x01": 0,"x02": 1,"x03": 1,"x04": 0,"y00": 1,"y01": 1,"y02": 1,"y03": 1,"y04": 1,}
#dat2 = [("ntg", "XOR", "fgs", "mjb"),("y02", "OR", "x01", "tnw"),("kwq", "OR", "kpj", "z05"),("x00", "OR", "x03", "fst"),("tgd", "XOR", "rvg", "z01"),("vdt", "OR", "tnw", "bfw"),("bfw", "AND", "frj", "z10"),("ffh", "OR", "nrd", "bqk"),("y00", "AND", "y03", "djm"),("y03", "OR", "y00", "psh"),("bqk", "OR", "frj", "z08"),("tnw", "OR", "fst", "frj"),("gnj", "AND", "tgd", "z11"),("bfw", "XOR", "mjb", "z00"),("x03", "OR", "x00", "vdt"),("gnj", "AND", "wpb", "z02"),("x04", "AND", "y00", "kjc"),("djm", "OR", "pbm", "qhw"),("nrd", "AND", "vdt", "hwm"),("kjc", "AND", "fst", "rvg"),("y04", "OR", "y02", "fgs"),("y01", "AND", "x02", "pbm"),("ntg", "OR", "kjc", "kwq"),("psh", "XOR", "fgs", "tgd"),("qhw", "XOR", "tgd", "z09"),("pbm", "OR", "djm", "kpj"),("x03", "XOR", "y03", "ffh"),("x00", "XOR", "y04", "ntg"),("bfw", "OR", "bqk", "z06"),("nrd", "XOR", "fgs", "wpb"),("frj", "XOR", "qhw", "z04"),("bqk", "OR", "frj", "z07"),("y03", "OR", "x01", "nrd"),("hwm", "AND", "bqk", "z03"),("tgd", "XOR", "rvg", "z12"),("tnw", "OR", "pbm", "gnj"),]

ops = {"AND": lambda a,b: a & b, "OR": lambda a,b: a|b, "XOR": lambda a,b: a^b}

vals = dict(dat)
todo = dat2
while todo:
	nexttodo = []
	for a, b, c, d in todo:
		try:
			vals[d] = ops[b](vals[a], vals[c])
		except KeyError:
			nexttodo.append((a,b , c, d))
	todo = nexttodo
#print({k:v for k,v in vals.items() if k.startswith("z")})
print(int(''.join(str(vals[f"z{i:02d}"]) for i in range(46))[::-1],2))

#dat2 = [(min(a,c),b,max(a,c),d) for a,b,c,d in dat2]
##dat2.sort(key=lambda x:(x[1],x[0],x[1],x[2]))
#dat2.sort()
#pprint(dat2)

#('x00', 'XOR', 'y00', 'z00'),
#('x00', 'AND', 'y00', 'wrs'),

#('x01', 'XOR', 'y01', 'wmq'),  x01 ^ y01 => intcarry1_01
#('wmq', 'XOR', 'wrs', 'z01'),  intcarry1_01 ^ extcarry_00 => z01
#('x01', 'AND', 'y01', 'rkd'),  x01 & y01 => intcarry2_01
#('wmq', 'AND', 'wrs', 'tfd'),  intcarry1_01 & extcarry_00  => intcarry3_01
#('rkd', 'OR', 'tfd', 'ktr'),   intcarry2_01 | intcarry3_01 => extcarry_01

#('x44', 'XOR', 'y44', 'nbf'),
#('gmg', 'XOR', 'nbf', 'z44'),
#('x44', 'AND', 'y44', 'nkv'),
#('gmg', 'AND', 'nbf', 'wmj'),
#('nkv', 'OR', 'wmj', 'z45'), ditto but extcarry_44 = z45

vartype = defaultdict(set)
varrestype = defaultdict(set)
for a, b, c, d in dat2:
	if {a,c} == {"x00", "y00"}:
		continue
	a,c = sorted([a, c])
	if a.startswith("x") and c.startswith("y"):
		if b == "XOR":
			varrestype[d].add("intcarry1")
		elif b == "AND":
			varrestype[d].add("intcarry2")
		else:
			varrestype[d].add("err1")
	elif b == "XOR":
		vartype[a].add("output")
		vartype[c].add("output")
		varrestype[d].add("output")
	elif b == "AND":
		vartype[a].add("intcarry3")
		vartype[c].add("intcarry3")
		varrestype[d].add("intcarry3")
	elif b == "OR":
		vartype[a].add("extcarry")
		vartype[c].add("extcarry")
		varrestype[d].add("extcarry")
	else:
		vartype[a].add("err2")
		vartype[c].add("err2")
		varrestype[d].add("err2")

for i in varrestype.keys():
	if vartype[i] == set() and i.startswith("z") and varrestype[i] == {"output"}:
		pass
	elif i == "z45" and varrestype[i] == {"extcarry"}:
		pass
	elif vartype[i] == {"output", "intcarry3"} and varrestype[i] == {"intcarry1"}:
		pass
	elif vartype[i] == {"extcarry"} and varrestype[i] == {"intcarry2"}:
		pass
	elif vartype[i] == {"extcarry"} and varrestype[i] == {"intcarry3"}:
		pass
	elif vartype[i] == {"output", "intcarry3"} and varrestype[i] == {"extcarry"}:
		pass
	else:
		print(i, vartype[i], varrestype[i])


