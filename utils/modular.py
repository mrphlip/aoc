from myutils import gcdext

__all__ = ['Modular']

class Modular(object):
	def __init__(self, n, base):
		if not isinstance(n, int) or not isinstance(base, int):
			raise TypeError("Values must be ints")
		self.n = n % base
		self.base = base

	def __eq__(self, other):
		if isinstance(other, int):
			other = Modular(other, self.base)
		elif isinstance(other, Modular):
			if self.base != other.base: raise ValueError("Cannot compare modular of different bases")
		else:
			return NotImplemented
		return self.n == other.n

	def __add__(self, other):
		if isinstance(other, Modular):
			if self.base != other.base: raise ValueError("Cannot compare modular of different bases")
			other = other.n
		if isinstance(other, int):
			return Modular(self.n + other, self.base)
		else:
			return NotImplemented
	__radd__ = __add__

	def __sub__(self, other):
		if isinstance(other, Modular):
			if self.base != other.base: raise ValueError("Cannot compare modular of different bases")
			other = other.n
		if isinstance(other, int):
			return Modular(self.n - other, self.base)
		else:
			return NotImplemented
	def __rsub__(self, other):
		if isinstance(other, Modular):
			if self.base != other.base: raise ValueError("Cannot compare modular of different bases")
			other = other.n
		if isinstance(other, int):
			return Modular(other - self.n, self.base)
		else:
			return NotImplemented

	def __mul__(self, other):
		if isinstance(other, Modular):
			if self.base != other.base: raise ValueError("Cannot compare modular of different bases")
			other = other.n
		if isinstance(other, int):
			return Modular(self.n * other, self.base)
		else:
			return NotImplemented
	__rmul__ = __mul__

	_recip_cache = {}
	@classmethod
	def _recip(cls, n, base, exc=True):
		if base not in cls._recip_cache:
			recips = [None] * base
			for i in range(1, base):
				if recips[i] is not None:
					continue
				g, _, _, x, _ = gcdext(i, base)
				if g == 1:
					recips[i] = x
					recips[-i] = -x
					recips[x] = i
					recips[-x] = -i
			cls._recip_cache[base] = recips

		if n == 0 and exc:
			raise ZeroDivisionError("division by zero")
		val = cls._recip_cache[base][n]
		if val is None and exc:
			raise ValueError("Divisor not coprime to base")
		return val

	def recip(self):
		return Modular(self._recip(self.n, self.base), self.base)
	def divisible(self):
		return self._recip(self.n, self.base, False) is not None

	def __div__(self, other):
		if isinstance(other, Modular):
			if self.base != other.base: raise ValueError("Cannot compare modular of different bases")
			other = other.n
		if isinstance(other, int):
			return Modular(self.n * self._recip(other, self.base), self.base)
		else:
			return NotImplemented
	__truediv__ = __div__
	def __rdiv__(self, other):
		if isinstance(other, Modular):
			if self.base != other.base: raise ValueError("Cannot compare modular of different bases")
			other = other.n
		if isinstance(other, int):
			return Modular(other * self._recip(self.n, self.base), self.base)
		else:
			return NotImplemented
	__rtruediv__ = __rdiv__

	def __pow__(self, other):
		if isinstance(other, int):
			acc = Modular(1, self.base)
			x = self
			while other:
				if other % 2:
					acc *= x
				x *= x
				other //= 2
			return acc
		else:
			return NotImplemented

	def __neg__(self):
		return Modular(-self.n, self.base)
	def __pos__(self):
		return self
	def __int__(self):
		return self.n
	def __float__(self):
		return float(self.n)

	def __repr__(self):
		return "Modular(%d, %d)" % (self.n, self.base)
	def __str__(self):
		return str(self.n)

	def __hash__(self):
		return hash(self.n)

	def __bool__(self):
		return bool(self.n)
	__nonzero__ = __bool__
