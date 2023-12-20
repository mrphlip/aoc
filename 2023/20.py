#!/usr/bin/python
dat = {
	"jx": ('%',("rt", "rs")),
	"cc": ('&',("cd", "fc", "qr", "nl", "gk", "zr")),
	"qs": ('%',("cl", "rs")),
	"zr": ('%',("cq",)),
	"mx": ('%',("nr", "pm")),
	"mj": ('%',("qr", "cc")),
	"cj": ('%',("cc", "nt")),
	"jv": ('%',("sp",)),
	"dj": ('%',("bd", "zc")),
	"kt": ('%',("lt",)),
	"broadcaster": ('b',("gz", "xg", "cd", "sg")),
	"dn": ('&',("rx",)),
	"br": ('%',("nf", "bd")),
	"cd": ('%',("cc", "nl")),
	"zc": ('%',("jq", "bd")),
	"xg": ('%',("cf", "pm")),
	"nz": ('%',("gm", "bd")),
	"dd": ('&',("dn",)),
	"nb": ('%',("sl",)),
	"pm": ('&',("kt", "xg", "xp", "jv", "sp")),
	"fh": ('&',("dn",)),
	"rt": ('%',("qq",)),
	"qq": ('%',("rs", "hd")),
	"hd": ('%',("qs", "rs")),
	"xp": ('&',("dn",)),
	"pj": ('%',("cc", "mj")),
	"gz": ('%',("bd", "kb")),
	"zd": ('%',("jv", "pm")),
	"cq": ('%',("cj", "cc")),
	"qr": ('%',("gk",)),
	"ng": ('%',("jk", "bd")),
	"kb": ('%',("bd", "sv")),
	"cl": ('%',("zx", "rs")),
	"gj": ('%',("zd", "pm")),
	"sl": ('%',("kx",)),
	"sv": ('%',("br",)),
	"nf": ('%',("bd", "nz")),
	"zx": ('%',("rs",)),
	"nt": ('%',("mn", "cc")),
	"rh": ('%',("nb", "rs")),
	"gk": ('%',("ln",)),
	"bd": ('&',("gm", "gz", "fh", "sv")),
	"jq": ('%',("ng", "bd")),
	"sp": ('%',("pc",)),
	"sg": ('%',("rs", "rh")),
	"kx": ('%',("jx",)),
	"fc": ('&',("dn",)),
	"cf": ('%',("gj", "pm")),
	"pc": ('%',("kt", "pm")),
	"jk": ('%',("bd",)),
	"vf": ('%',("pm",)),
	"rs": ('&',("sg", "dd", "sl", "kx", "nb", "rt")),
	"nr": ('%',("vf", "pm")),
	"ln": ('%',("zr", "cc")),
	"lt": ('%',("pm", "mx")),
	"gm": ('%',("dj",)),
	"nl": ('%',("pj",)),
	"mn": ('%',("cc",)),
}

#dat = {"broadcaster":("b",('a','b','c')),'a':('%',('b',)),'b':('%',('c',)),'c':('%',('inv',)),'inv':('&',('a',)),}

dat['rx'] = ('*', ())

inc = {k:[] for k in dat.keys()}
for k,v in dat.items():
	for i in v[1]:
		inc[i].append(k)

flipflops = {}
ands = {}
def reset():
	global flipflops, ands
	flipflops = {k:False for k,v in dat.items() if v[0] == '%'}
	ands = {k:{i:False for i in inc[k]} for k,v in dat.items() if v[0] == '&'}
reset()

lowpulses = 0
highpulses = 0
def run(stepnum):
	global lowpulses, highpulses
	rxseen = False
	todo = [('broadcaster', False, '')]
	while todo:
		node, level, prevnode = todo.pop(0)
		if level:
			highpulses += 1
		else:
			lowpulses += 1
		mode, links = dat[node]
		if mode == 'b':
			todo.extend([(n, level, node) for n in links])
		elif mode == '%':
			if not level:
				flipflops[node] = not flipflops[node]
				todo.extend([(n, flipflops[node], node) for n in links])
		elif mode == '&':
			ands[node][prevnode] = level
			if node == 'dn' and level:
				print(prevnode, stepnum)
			v = not all(ands[node].values())
			todo.extend([(n, v, node) for n in links])
		elif mode == '*':
			if not level:
				rxseen = True
	return rxseen

for i in range(1000):
	run(i)
print(lowpulses * highpulses)

reset()
i = 1
while not run(i):
	i += 1
print(i)
