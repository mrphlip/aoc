import sys, os
sys.path.insert(1, os.path.join(os.path.dirname(__file__), "..", "utils"))
from collections import defaultdict, Counter
from fractions import Fraction
from copy import deepcopy
from pprint import pprint
import re
from math import factorial as fact, gcd, lcm, isqrt, perm as npr, comb as ncr, prod
from math import sqrt, sin, cos, tan, atan, pi, ceil, floor
from itertools import count, cycle, repeat, chain, groupby, product, permutations, combinations
from functools import cache, cmp_to_key, partial, reduce

from myutils import crt, gcdext, primes, primes_to, factor
from matrix import Matrix
from vector import Vector
from modular import Modular
from prioqueue import PrioQueue
from dijkstra import dijkstra, dijkstra_grid
from ranges import Ranges
