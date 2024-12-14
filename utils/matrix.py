from vector import BaseVector, Vector, ConstVector

__all__  = ['Matrix', 'ConstMatrix']

class BaseMatrix(object):
	VECTORCLASS = BaseVector
	def __init__(self, *args):
		if len(args) == 1 and hasattr(args[0], '__iter__'):
			args = args[0]
		self.data = [self.VECTORCLASS(row) for row in args]
		if self.data:
			self.height = len(self.data)
			self.width = len(self.data[0])
			for row in self.data:
				if len(row) != self.width:
					raise ValueError("Inconsistent widths")
		else:
			self.width = self.height = 0
		self._transpdata = None

	@property
	def transpdata(self):
		if not self._transpdata:
			self._transpdata = [self.VECTORCLASS(col) for col in zip(*self)]
		return self._transpdata

	def rows(self):
		return iter(self.data)
	def cols(self):
		return iter(self.transpdata)
	def __iter__(self):
		return self.rows()
	def __len__(self):
		return len(self.data)
	def __getitem__(self, ix):
		if isinstance(ix, tuple) and len(ix) == 2:
			row, col = ix
			return self.data[row][col]
		else:
			return self.data[ix]
	def __reversed__(self):
		return reversed(self.data)

	def transpose(self):
		return type(self)(self.transpdata)

	def __add__(self, other):
		if self.width != other.width or self.height != other.height:
			raise ValueError("Attempted to add matrices of different size")
		return type(self)(i+j for i,j in zip(self, other))
	__radd__ = __add__

	def __sub__(self, other):
		if self.width != other.width or self.height != other.height:
			raise ValueError("Attempted to subtract matrices of different size")
		return type(self)(i-j for i,j in zip(self, other))
	def __rsub__(self, other):
		if self.width != other.width or self.height != other.height:
			raise ValueError("Attempted to subtract matrices of different size")
		return type(self)(j-i for i,j in zip(self, other))

	def __mul__(self, other):
		if isinstance(other, BaseMatrix):
			isvector = False
		elif hasattr(other, '__iter__'):
			# Treat Matrix * Vector as multiplying by a column vector
			other = type(self)([[i] for i in other])
			isvector = True
		else:
			# scalar mult
			return type(self)(row * other for row in self)
		if self.width != other.height:
			raise ValueError("Attempted to multiply matricies of mismatched size")
		ret = [
			[
				row * col
				for col in other.cols()
			]
			for row in self.rows()
		]
		if isvector:
			return self.VECTORCLASS([i for i, in ret])
		else:
			return type(self)(ret)
	def __rmul__(self, other):
		if isinstance(other, BaseMatrix):
			isvector = False
		elif hasattr(other, '__iter__'):
			# Treat Vector * Matrix as multiplying by a row vector
			other = type(self)([other])
			isvector = True
		else:
			# scalar mult
			return type(self)(row * other for row in self)
		if other.width != self.height:
			raise ValueError("Attempted to multiply matricies of mismatched size")
		ret = [
			[
				row * col
				for col in self.cols()
			]
			for row in other.rows()
		]
		if isvector:
			return self.VECTORCLASS(ret[0])
		else:
			return type(self)(ret)

	def __div__(self, other):
		if hasattr(other, '__iter__'):
			raise TypeError("Cannot divide by a matrix")
		return type(self)(i/other for i in self)
	__truediv__ = __div__
	def __rdiv__(self, other):
		raise TypeError("Cannot divide by a matrix")
	__rtruediv__ = __rdiv__

	def __neg__(self):
		return type(self)(-i for i in self)
	def __pos__(self):
		return type(self)(self)

	def __str__(self):
		data = [[str(i) for i in row] for row in self]
		colwid = [max(len(row[i]) for row in data) for i in range(self.width)]
		return "\n".join(
			'[%s]' % ' '.join(
				cell.rjust(width) for width, cell in zip(colwid, row)
			) for row in data
		)
	def __repr__(self):
		return "%s(%r)" % (type(self).__name__, [list(i) for i in self])

	def __eq__(self, other):
		return len(self) == len(other) and all(i == j for i,j in zip(self, other))

	@classmethod
	def identity(cls, size, zero=0, one=1):
		return cls([[one if i == j else zero for j in range(size)] for i in range(size)])

	def solve(self, other):
		if self.width != self.height:
			raise ValueError("Not square")
		if self.height != len(other):
			raise ValueError("Matrices not same height")
		# make scratch lists
		scratch = [ConstVector(i) for i in self]
		if isinstance(other, BaseVector):
			other = [ConstVector([i]) for i in other]
			isvector = True
		else:
			other = [ConstVector(i) for i in other]
			isvector = False
		# solve forward
		for x in range(self.width):
			# find a non-zero value in this column
			found_indivisible = False
			for y in range(x, self.height):
				if scratch[y][x]:
					if not hasattr(scratch[y][x], 'divisible') or scratch[y][x].divisible():
						break
					else:
						found_indivisible = True
			else:
				if found_indivisible:
					#print(Matrix(scratch))
					#print()
					#print(Matrix(other))
					raise ValueError("Matrix column is indivisible")
				else:
					raise ValueError("Matrix is singular")
			if x != y:
				other[x], other[y] = other[y], other[x]
				scratch[x], scratch[y] = scratch[y], scratch[x]
			other[x] = other[x] / scratch[x][x]
			scratch[x] = scratch[x] / scratch[x][x]
			for y in range(x+1, self.height):
				other[y] = other[y] - scratch[y][x] * other[x]
				scratch[y] = scratch[y] - scratch[y][x] * scratch[x]
		# back-solve
		for x in range(self.width-1, -1, -1):
			for y in range(x):
				other[y] = other[y] - scratch[y][x] * other[x]
				scratch[y] = scratch[y] - scratch[y][x] * scratch[x]
		if isvector:
			return self.VECTORCLASS([i for i, in other])
		else:
			return type(self)(other)

	def invert(self, zero=0, one=1):
		return self.solve(self.identity(self.height, zero, one))

class Matrix(BaseMatrix):
	VECTORCLASS = Vector
	def __iadd__(self, other):
		if self.width != other.width or self.height != other.height:
			raise ValueError("Attempted to add matrices of different size")
		self.data = [i+j for i,j in zip(self, other)]
		self._transpdata = None
		return self
	def __isub__(self, other):
		if self.width != other.width or self.height != other.height:
			raise ValueError("Attempted to subtract matrices of different size")
		self.data = [i-j for i,j in zip(self, other)]
		self._transpdata = None
		return self
	def __imul__(self, other):
		# cba to do this in-place
		new = self * other
		self.data = new.data
		self.width = new.width
		self.height = new.height
		self._transpdata = None
	def __idiv__(self, other):
		new = self / other
		self.data = new.data
		self.width = new.width
		self.height = new.height
		self._transpdata = None
	__itruediv__ = __idiv__
	def __setitem__(self, ix, val):
		if isinstance(ix, tuple) and len(ix) == 2:
			row, col = ix
			self.data[row][col] = val
		else:
			self.data[ix] = val
		self._transpdata = None

class ConstMatrix(BaseMatrix):
	VECTORCLASS = ConstVector
	def __hash__(self):
		hash = 0
		for i in self:
			hash = (hash & 0xFFFFFF) << 8 | (hash & 0xFF000000) >> 24
			hash ^= i.__hash__()
		return hash
