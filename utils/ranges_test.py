from ranges import Ranges

def test_merging():
	a = Ranges([(1, 5), (5, 10), (20, 30), (40, 50), (46, 48), (28, 45), (100, 100), (300, 200)])
	assert a.ranges == ((1, 10), (20, 50))

def test_contains():
	a = Ranges([(1, 10), (20, 30), (40, 50)])
	assert 0 not in a
	assert 1 in a
	assert 10 not in a
	assert 25 in a
	assert 35 not in a
	assert 45 in a
	assert 50 not in a
	assert 100 not in a

def test_intersect():
	a = Ranges([(1, 10), (20, 30), (40, 50)])
	b = Ranges([(5, 25)])
	assert (a & b).ranges == ((5, 10), (20, 25))
	assert (b & a).ranges == ((5, 10), (20, 25))

def test_union():
	a = Ranges([(1, 10), (20, 30), (40, 50)])
	b = Ranges([(5, 25)])
	assert (a | b).ranges == ((1, 30), (40, 50))
	assert (b | a).ranges == ((1, 30), (40, 50))

def test_difference():
	a = Ranges([(1, 10), (20, 30), (40, 50)])
	b = Ranges([(5, 25)])
	assert (a - b).ranges == ((1, 5), (25, 30), (40, 50))
	assert (b - a).ranges == ((10, 20),)
	assert (a ^ b).ranges == ((1, 5), (10, 20), (25, 30), (40, 50))

def test_subset():
	a = Ranges([(1, 10), (20, 30)])
	b = Ranges([(2, 8), (22, 28)])
	assert b <= a
	assert b < a
	assert a >= b
	assert a > b
	assert not (a <= b)
	assert not (a < b)
	assert not (b >= a)
	assert not (b > a)
	assert a <= a
	assert not (a < a)

def test_len():
	a = Ranges([(1, 10), (20, 30), (40, 50)])
	assert a
	assert len(a) == 29

	b = Ranges([])
	assert not b
	assert len(b) == 0

def test_misc():
	a = Ranges([(1, 10), (20, 30), (40, 50)])
	assert list(iter(a)) == [(1, 10), (20, 30), (40, 50)]
	assert list(reversed(a)) == [(40, 50), (20, 30), (1, 10)]
	assert a[0] == (1, 10)
	assert a[1] == (20, 30)
	assert a[2] == (40, 50)
	assert hash(a) == hash(((1, 10), (20, 30), (40, 50)))
	assert a == a
	assert a != Ranges([])
	assert str(a) == "1-10, 20-30, 40-50"
	assert repr(a) == "Ranges(((1, 10), (20, 30), (40, 50)))"
