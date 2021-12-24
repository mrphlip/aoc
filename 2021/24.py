dat = [
	("inp", 'w',),("mul", 'x', 0),("add", 'x', 'z'),("mod", 'x', 26), ...
]

def dm(a,b):
	d = a // b
	m = a % b
	if d < 0 and m != 0:
		d += 1
		m -= b
	return d,m

def run(input):
	vars = {k:0 for k in "wxyz"}
	def p(x):
		if isinstance(x, int):
			return x
		else:
			return vars[x]
	for opcode, *params in dat:
		if opcode == "inp":
			vars[params[0]] = input.pop(0)
		elif opcode == "add":
			vars[params[0]] += p(params[1])
		elif opcode == "mul":
			vars[params[0]] *= p(params[1])
		elif opcode == "div":
			if p(params[1]) == 0:
				raise "div by zero"
			vars[params[0]] = dm(p(params[0]), p(params[1]))[0]
		elif opcode == "mod":
			if p(params[1]) <= 0 or p(params[0]) < 0:
				raise "mod by invalid args"
			vars[params[0]] = dm(p(params[0]), p(params[1]))[1]
		elif opcode == "eql":
			vars[params[0]] = 1 if p(params[0]) == p(params[1]) else 0
	return vars['z'] == 0

#from itertools import product
#for inp in product("987654321", repeat=14):
#	if run([int(i) for i in inp]):
#		print(''.join(inp))
#		break
print(run([int(i) for i in "59996912981939"]))
print(run([int(i) for i in "17241911811915"]))
