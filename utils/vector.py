from math import sqrt

__all__ = ['Vector', 'ConstVector']

class BaseVector(object):
	def __init__(self, *args):
		if len(args) == 1 and hasattr(args[0], '__iter__'):
			args = args[0]
		self._coords = list(args)

	def __add__(self, other):
		if len(self) != len(other): raise ValueError("Attempted to add vectors of different length")
		return type(self)(i+j for i,j in zip(self, other))
	__radd__ = __add__

	def __sub__(self, other):
		if len(self) != len(other): raise ValueError("Attempted to subtract vectors of different length")
		return type(self)(i-j for i,j in zip(self, other))
	def __rsub__(self, other):
		if len(self) != len(other): raise ValueError("Attempted to subtract vectors of different length")
		return type(self)(j-i for i,j in zip(self, other))

	def __mul__(self, other):
		""" scalar*vector = scalar multiplication
		vector*vector = dot product """
		if hasattr(other, '__iter__'):
			if len(self) != len(other): raise ValueError("Attempted to dot-product vectors of different length")
			return sum(i*j for i,j in zip(self, other))
		else:
			return type(self)(i*other for i in self)
	__rmul__ = __mul__
	def cross(self, other):
		if len(self) != 3 or len(other) != 3: raise ValueError("Cross product only defined for vectors of length 3")
		return type(self)(
			self[1] * other[2] - self[2] * other[1],
			self[2] * other[0] - self[0] * other[2],
			self[0] * other[1] - self[1] * other[0],
		)

	def __div__(self, other):
		if hasattr(other, '__iter__'):
			raise TypeError("Cannot divide by a vector")
		return type(self)(i/other for i in self)
	__truediv__ = __div__
	def __rdiv__(self, other):
		raise TypeError("Cannot divide by a vector")
	__rtruediv__ = __rdiv__

	def __neg__(self):
		return type(self)(-i for i in self)
	def __pos__(self):
		return type(self)(self)
	def __abs__(self):
		return sqrt(sum(i*i for i in self))

	def __str__(self):
		return "<%s>" % ', '.join(str(i) for i in self)
	def __repr__(self):
		return "%s(%s)" % (type(self).__name__, ', '.join(repr(i) for i in self))
	def squaredist(self):
		return sum(i*i for i in self)
	def norm(self, length=1):
		return self * length / abs(self)

	def __eq__(self, other):
		return len(self) == len(other) and all(i == j for i,j in zip(self, other))
	__hash__ = None

	def __nonzero__(self):
		return any(self)

	def __len__(self):
		return len(self._coords)
	def __getitem__(self, ix):
		return self._coords[ix]
	def __iter__(self):
		return iter(self._coords)
	def __reversed__(self):
		return reversed(self._coords)

	def project(self, other):
		"""
		Projects other onto self

		Returns a vector in the same direction as self, but with a length of
		the component of other, parallel to self
		"""
		return ((self * other) / self.squaredist()) * self

class Vector(BaseVector):
	def __iadd__(self, other):
		if len(self) != len(other): raise ValueError("Attempted to add vectors of different length")
		self._coords = [i+j for i,j in zip(self, other)]
		return self
	def __isub__(self, other):
		if len(self) != len(other): raise ValueError("Attempted to subtract vectors of different length")
		self._coords = [i-j for i,j in zip(self, other)]
		return self
	def __imul__(self, other):
		if hasattr(other, '__iter__'):
			raise TypeError("Cannot use *= for dot products")
		else:
			self._coords = [i*other for i in self]
		return self
	def __idiv__(self, other):
		self._coords = [i/other for i in self]
		return self
	__itruediv__ = __idiv__
	def __setitem__(self, ix, val):
		self._coords[ix] = val

class ConstVector(BaseVector):
	def __hash__(self):
		hash = 0
		for i in self:
			hash = (hash & 0xFFFFFF) << 8 | (hash & 0xFF000000) >> 24
			hash ^= i.__hash__()
		return hash
