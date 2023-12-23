#!/usr/bin/python
from collections import Counter

dat = [
	("398KA", 456),
	("2J299", 282),
	("8939K", 547),
	[...snip...]
]

#dat = [("32T3K", 765),("T55J5", 684),("KK677", 28),("KTJJT", 220),("QQQJA", 483),]

cardval = {c:i for i,c in enumerate("23456789TJQKA")}
joker = cardval['J']

def rank(hand, jokers=False):
	hand = [cardval[i] for i in hand]
	h = Counter(hand)
	if jokers:
		hand = [-1 if i == joker else i for i in hand]
		numjokers = h.pop(joker, 0)
	h = [n for c,n in h.items()]
	h.sort()
	if jokers:
		if h:
			h[-1] += numjokers
		else:
			h.append(numjokers)
	
	match h:
		case [5]:
			return (7, *hand)
		case [1, 4]:
			return (6, *hand)
		case [2, 3]:
			return (5, *hand)
		case [1, 1, 3]:
			return (4, *hand)
		case [1, 2, 2]:
			return (3, *hand)
		case [1, 1, 1, 2]:
			return (2, *hand)
		case [1, 1, 1, 1, 1]:
			return (1, *hand)

d = list(dat)
d.sort(key=lambda x:rank(x[0]))
print(sum(i*b for i, (_, b) in enumerate(d, 1)))

d = list(dat)
d.sort(key=lambda x:rank(x[0], True))
print(sum(i*b for i, (_, b) in enumerate(d, 1)))
