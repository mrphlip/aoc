__all__ = ["product", "fact", "npr", "ncr", "gcdext", "gcd", "lcm", "crt", "primes", "factor"]

def product(seq, start=1):
	for n in seq:
		start *= n
	return start

def fact(n):
	return product(range(1, n+1))

def npr(n, r):
	return product(range(n-r+1, n+1))

def ncr(n, r):
	return npr(n,r)//fact(r)

def gcdext(a, b):
	"""
	Extended Euclidian algorithm

	g, a, b, p, q = gcdext(A, B)

	Such that:
	* g is gcd(A, B)
	* a and b are A//g and B//g
	* p and q are such that pA + qB = g

	In particular, if A and M are co-prime
	then:
	g, _, _, x, _ = gcdext(A, M)
	will result in:
	* g == 1
	* x == A^(-1) mod M
	"""
	s = 0
	s_ = 1
	t = 1
	t_ = 0
	r = b
	r_ = a
	while r:
		quot = r_ // r
		r_, r = r, r_ - quot * r
		s_, s = s, s_ - quot * s
		t_, t = t, t_ - quot * t
	return r_, t, s, s_, t_

def gcd(a, b):
	return gcdext(a, b)[0]

def lcm(a, b):
	return a * b // gcd(a, b)

def _crt(a1, m1, a2, m2):
	"""
	Chinese Remainder theorem

	a, m = crt(a1, m1, a2, m2)

	m1, m2 must be coprime
	Such that: for any number x = a (mod m)
		x = a1 (mod m1)
		x = a2 (mod m2)
	"""
	g, _, _, x1, x2 = gcdext(m1, m2)
	if g != 1:
		raise ValueError("Moduli must be coprime: %r, %r" % (m1, m2))
	newm = m1 * m2
	return (a1 * x2 * m2 + a2 * x1 * m1) % newm, newm

def crt(a1, m1, a2, m2):
	# Extended CRT for handling when the moduli are not coprime

	m = gcd(m1, m2)
	if m == 1:
		return _crt(a1, m1, a2, m2)

	if a1 % m != a2 % m:
		raise ValueError("Offsets are not in sync")
	ofs = a1 % m

	resa, resm = _crt((a1 - ofs) // m, m1 // m, (a2 - ofs) // m, m2 // m)
	return resa * m + ofs, resm * m

def primes():
	# https://stackoverflow.com/a/19391111
	import itertools
	yield 2; yield 3; yield 5; yield 7
	D = {}
	ps = primes()
	next(ps)
	p = next(ps)
	assert p == 3
	psq = p*p
	for i in itertools.count(9, 2):
		if i in D:      # composite
			step = D.pop(i)
		elif i < psq:   # prime
			yield i
			continue
		else:           # composite, = p*p
			assert i == psq
			step = 2*p
			p = next(ps)
			psq = p*p
		i += step
		while i in D:
			i += step
		D[i] = step

def primes_to(n):
	for i in primes():
		if i >= n:
			break
		yield i

def factor(n, limit=None, unlimit=0):
	if n <= 0:
		raise ValueError("Value must be positive non-zero")
	for p in primes():
		if limit is not None and unlimit <= 0 and p > limit:
			yield n
			return
		c = 0
		while True:
			d, m = divmod(n, p)
			if m != 0:
				break
			n = d
			c += 1
		if c > 0:
			yield p, c
			if limit is not None and p > limit:
				unlimit -= 1
		if n <= 1:
			break
