class Ranges:
	__slots__ = ["_ranges"]

	def __init__(self, ranges=()):
		flattened = []
		for i, j in sorted(ranges):
			if i >= j:
				continue
			elif not flattened or i > flattened[-1][1]:
				flattened.append((i, j))
			else:
				flattened[-1] = (flattened[-1][0], max(flattened[-1][1], j))
		self._ranges = tuple(flattened)

	@property
	def ranges(self):
		return self._ranges

	def find_range(self, val):
		min = 0
		max = len(self._ranges)
		while min < max:
			mid = (min + max) // 2
			i, j = self._ranges[mid]
			if i <= val < j:
				return i, j
			elif val < i:
				max = mid
			else:
				min = mid + 1
		return None

	# Set-like operations
	def __contains__(self, val):
		return self.find_range(val) is not None

	def __and__(self, other):
		if not isinstance(other, Ranges):
			return NotImplemented
		iter1 = iter(self)
		iter2 = iter(other)
		intersect = []
		try:
			i1, j1 = next(iter1)
			i2, j2 = next(iter2)
		except StopIteration:
			return Ranges()
		while True:
			if j1 <= i2:
				try:
					i1, j1 = next(iter1)
				except StopIteration:
					break
			elif j2 <= i1:
				try:
					i2, j2 = next(iter2)
				except StopIteration:
					break
			else:
				intersect.append((max(i1, i2), min(j1, j2)))
				i1 = i2 = min(j1, j2)
		return Ranges(intersect)
	def intersection(self, other):
		return self & other

	def __or__(self, other):
		if not isinstance(other, Ranges):
			return NotImplemented
		return Ranges(self.ranges + other.ranges)
	def union(self, other):
		return self | other

	def __sub__(self, other):
		if not isinstance(other, Ranges):
			return NotImplemented
		iter1 = iter(self)
		iter2 = iter(other)
		diff = []
		try:
			i1, j1 = next(iter1)
		except StopIteration:
			return Ranges()
		try:
			i2, j2 = next(iter2)
		except StopIteration:
			return self
		while True:
			if j1 <= i2:
				diff.append((i1, j1))
				try:
					i1, j1 = next(iter1)
				except StopIteration:
					break
			elif j2 <= i1:
				try:
					i2, j2 = next(iter2)
				except StopIteration:
					diff.append((i1, j1))
					diff.extend(iter1)
					break
			else:
				if i1 < i2:
					diff.append((i1, i2))
				i1 = i2 = min(j1, j2)
		return Ranges(diff)
	def difference(self, other):
		return self - other
	def __xor__(self, other):
		if not isinstance(other, Ranges):
			return NotImplemented
		return (self - other) | (other - self)
	def symmetric_difference(self, other):
		return self ^ other

	def isdisjoint(self, other):
		if not isinstance(other, Ranges):
			return NotImplemented
		return not (self & other)

	def __le__(self, other):
		if not isinstance(other, Ranges):
			return NotImplemented
		return (self & other) == self
	def __lt__(self, other):
		if not isinstance(other, Ranges):
			return NotImplemented
		return self != other and (self & other) == self
	def __ge__(self, other):
		if not isinstance(other, Ranges):
			return NotImplemented
		return (self & other) == other
	def __gt__(self, other):
		if not isinstance(other, Ranges):
			return NotImplemented
		return self != other and (self & other) == other
	def issubset(self, other):
		return self <= other
	def issuperset(self, other):
		return self >= other

	# Other misc operations
	def __bool__(self):
		return bool(self._ranges)

	def __len__(self):
		return sum(j-i for i, j in self._ranges)
	def __getitem__(self, ix):
		return self._ranges[ix]
	def __iter__(self):
		return iter(self._ranges)
	def __reversed__(self):
		return reversed(self._ranges)

	def __eq__(self, other):
		if not isinstance(other, Ranges):
			return NotImplemented
		return self._ranges == other._ranges
	def __hash__(self):
		return hash(self._ranges)

	def __str__(self):
		return ", ".join(f"{i}-{j}" for i, j in self._ranges)
	def __repr__(self):
		return f"Ranges({self._ranges!r})"
