#!/usr/bin/python
from collections import defaultdict, Counter
from fractions import Fraction
from copy import deepcopy
from pprint import pprint
import re, math

dat = """what()who(){from(),'mul(28,510)?<,>where()why()mul(...etc..."""

n = 0
for a, b in re.findall(r"mul\((\d+),(\d+)\)", dat):
	n += int(a) * int(b)
print(n)

n = 0
enabled = True
for a, b, c in re.findall(r"mul\((\d+),(\d+)\)|(do(?:n't)?)\(\)", dat):
	if c == "do":
		enabled = True
	elif c == "don't":
		enabled = False
	elif enabled:
		n += int(a) * int(b)
print(n)
