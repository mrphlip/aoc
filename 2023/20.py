#!/usr/bin/python
dat = {
	"jx": ('%',("rt", "rs")),
	"cc": ('&',("cd", "fc", "qr", "nl", "gk", "zr")),
	"qs": ('%',("cl", "rs")),
	[...snip...]
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
