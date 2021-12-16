from io import StringIO
dat = """4057231006[...]02A9336C20CE84"""
#dat = "EE00D40C823060"
b = lambda n: ("0000"+bin(n)[2:])[-4:]
dat = StringIO(''.join(b(int(n,16)) for n in dat))
ver_total = 0
def readpacket():
	v = int(dat.read(3),2)
	global ver_total
	ver_total += v
	t = int(dat.read(3),2)
	if t == 4:
		n = readnum()
		return (v,t,n)
	else:
		i = int(dat.read(1),2)
		if i == 0:
			l = int(dat.read(15),2)
			target = dat.tell() + l
			packets = []
			while dat.tell() < target:
				packets.append(readpacket())
		else:
			n = int(dat.read(11),2)
			packets = [readpacket() for i in range(n)]
		return (v,t,packets)
def readnum():
	n = 0
	cont = True
	while cont:
		cont = int(dat.read(1),2)
		n = (n << 4) + int(dat.read(4),2)
	return n
packet = readpacket()
print(ver_total)
def eval(pack):
	v,t,val = pack
	if t == 0:
		return sum(eval(i) for i in val)
	elif t == 1:
		ret = 1
		for i in val:
			ret *= eval(i)
		return ret
	elif t == 2:
		return min(eval(i) for i in val)
	elif t == 3:
		return max(eval(i) for i in val)
	elif t == 4:
		return val
	elif t == 5:
		return 1 if eval(val[0]) > eval(val[1]) else 0
	elif t == 6:
		return 1 if eval(val[0]) < eval(val[1]) else 0
	elif t == 7:
		return 1 if eval(val[0]) == eval(val[1]) else 0
print(eval(packet))
