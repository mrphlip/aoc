dat = ["DACA","DCBB"]

POS = [0,1,3,5,7,9,10]

class State:
	def __init__(self, depth, map):
		self.depth = depth
		self.map = tuple(tuple(i) if not isinstance(i,tuple) else i for i in map)
	def __repr__(self):
		return f"State({self.depth}, {self.map})"
	def __str__(self):
		return f"#############\n#{self._chr(4,0)}{self._chr(4,1)}.{self._chr(4,2)}.{self._chr(4,3)}.{self._chr(4,4)}.{self._chr(4,5)}{self._chr(4,6)}#\n" + "".join(
			f"{'##' if i == 0 else '  '}#{self._chr(0,i)}#{self._chr(1,i)}#{self._chr(2,i)}#{self._chr(3,i)}#{'##' if i == 0 else '  '}\n" for i in range(self.depth)
		) + "  #########  "
	def _chr(self, a, b):
		if self.map[a][b] is None:
			return "."
		else:
			return chr(self.map[a][b] + 65)
	def __hash__(self):
		return hash(self.map)
	def __eq__(self, other):
		return self.map == other.map
	def neighbours(self):
		for i in range(4):
			for j in range(self.depth):
				if self.map[i][j] is not None:
					break
			else:
				continue
			for k in range(i+1,-1,-1):
				if self.map[4][k] is not None:
					break
				yield self.newstate(i,j,4,k)
			for k in range(i+2,7):
				if self.map[4][k] is not None:
					break
				yield self.newstate(i,j,4,k)
		for i in range(7):
			if self.map[4][i] is None:
				continue
			target = self.map[4][i]
			if not all(j is None or j == target for j in self.map[target]):
				continue
			if i <= target+1:
				if not all(j is None for j in self.map[4][i+1:target+2]):
					continue
			else:
				if not all(j is None for j in self.map[4][target+2:i]):
					continue
			k = -1
			while k+1 < self.depth and self.map[target][k+1] is None:
				k += 1
			if k < 0:
				continue
			yield self.newstate(4,i,target,k)
	@staticmethod
	def dist(froma,fromb,toa,tob):
		if froma == 4 and toa == 4:
			return abs(POS[fromb] - POS[tob])
		elif froma == 4:
			return abs(POS[fromb] - 2*(toa+1)) + tob+1
		elif toa == 4:
			return fromb+1 + abs(2*(froma+1) - POS[tob])
		else:
			return fromb+1 + abs(2*(froma+1) - 2*(toa+1)) + tob+1
	def newstate(self,froma,fromb,toa,tob):
		newmap = list(self.map)
		newmap[froma] = list(newmap[froma])
		newmap[toa] = list(newmap[toa])
		weight = newmap[toa][tob] = newmap[froma][fromb]
		newmap[froma][fromb] = None
		return State(self.depth, newmap), self.dist(froma,fromb,toa,tob) * 10**weight
	def isfinal(self):
		return all(self.map[i][j] == i for i in range(4) for j in range(self.depth)) and all(self.map[4][i] is None for i in range(7))
	def heuristic(self):
		def gen():
			seen = [0,0,0,0]
			for i in range(4):
				j = self.depth - 1
				while j >= 0 and self.map[i][j] == i:
					seen[i] += 1
					j -= 1
			for i,group in enumerate(self.map):
				for j,x in enumerate(group):
					if x is not None and (i == 4 or not all(self.map[i][k] == i for k in range(j,self.depth))):
						seen[x] += 1
						yield self.dist(i,j,x,self.depth-seen[x]) * 10**x
		return sum(gen())

def make_initstate(dat):
	dat = [[ord(i) - 65 for i in j] for j in dat]
	return State(len(dat), (*zip(*dat), [None] * 7))

def go_dijkstra(initstate):
	statemap = {initstate: 0}
	candidates = {initstate}
	while candidates:
		minstate = min(candidates, key=statemap.__getitem__)
		if minstate.isfinal():
			return statemap[minstate]
		candidates.remove(minstate)
		nodecost = statemap[minstate]
		for n, c in minstate.neighbours():
			if n not in statemap or statemap[n] > nodecost + c:
				statemap[n] = nodecost + c
				candidates.add(n)
	else:
		return None

def go_astar(initstate):
	statemap = {initstate: 0}
	score = {initstate: initstate.heuristic()}
	candidates = {initstate}
	while candidates:
		minstate = min(candidates, key=score.__getitem__)
		if minstate.isfinal():
			return statemap[minstate]
		candidates.remove(minstate)
		nodecost = statemap[minstate]
		for n, c in minstate.neighbours():
			if n not in statemap or statemap[n] > nodecost + c:
				statemap[n] = nodecost + c
				score[n] = nodecost + c + n.heuristic()
				candidates.add(n)
	else:
		return None

#print(go_dijkstra(make_initstate(dat)))
print(go_astar(make_initstate(dat)))
print(go_astar(make_initstate([dat[0], "DCBA", "DBAC", dat[1]])))
