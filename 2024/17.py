#!/usr/bin/python
from aocimports import *

dat = [
	2,4,1,[...]
]
A, B, C = 627[...], 0, 0
outputs = []

#dat = [0,1,5,4,3,0]
#A = 729

def proc(opcode, operand):
	global A, B, C
	#print(opcode, operand, A, B, C)
	if operand < 4:
		combo = operand
	else:
		combo = [A, B, C, None][operand - 4]
	if opcode == 0:
		A = A >> combo
	elif opcode == 1:
		B ^= operand
	elif opcode == 2:
		B = combo % 8
	elif opcode == 3:
		if A:
			return operand
	elif opcode == 4:
		B ^= C
	elif opcode == 5:
		outputs.append(combo % 8)
	elif opcode == 6:
		B = A >> combo
	elif opcode == 7:
		C = A >> combo

def run():
	ip = 0
	while ip < len(dat):
		ret = proc(dat[ip], dat[ip+1])
		if ret is None:
			ip += 2
		else:
			ip = ret

run()
print(outputs)

def mkbits(ix, n):
	yield ix, n & 1
	yield ix + 1, (n & 2)>>1
	yield ix + 2, (n & 4)>>2
def testbits(bits, ix, n):
	for bitix, bit in mkbits(ix, n):
		if len(bits) <= bitix or bits[bitix] is None or bits[bitix] == bit:
			pass
		else:
			return False
	return True
def setbits(bits, ix, n):
	bits = bits[:]
	while len(bits) < ix + 3:
		bits.append(None)
	for bitix, bit in mkbits(ix, n):
		bits[bitix] = bit
	return bits
def resolve(target, bits, i):
	if i >= len(target):
		n = 0
		for bit in bits[::-1]:
			n = n * 2 + (bit or 0)
		yield n
		return
	for x in range(8):
		y = target[i] ^ x
		if testbits(bits, i*3, x) and testbits(bits, i*3+(7^x), y):
			newbits = setbits(setbits(bits, i*3, x), i*3+(7^x), y)
			yield from resolve(target, newbits, i+1)

def find(targets):
	global A, B, C, outputs
	for i in resolve(targets, [], 0):
		A, B, C = i, 0, 0
		outputs = []
		run()
		if outputs == targets:
			yield i

print(min(find(dat)))
