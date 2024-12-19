__all__ = ["dijkstra", "dijkstra_grid"]

from prioqueue import PrioQueue

def dijkstra(start_pos, neighbours, target=None, heuristic=None):
	distmap = {}
	todo = PrioQueue()
	todo[start_pos] = 0, (0, None)
	if target is not None and not isinstance(target, set):
		target = {target}
	while todo and not (target and target <= distmap.keys()):
		node, _, (dist, prev) = todo.pop()
		distmap[node] = dist, prev
		for n, ndist in neighbours(node):
			if n in distmap:
				continue
			newscore = dist + ndist
			if heuristic:
				newscore += heuristic(n)
			if n not in todo or todo[n][0] > newscore:
				todo[n] = newscore, (dist + ndist, {node})
			elif todo[n][0] == newscore:
				todo[n][1][1].add(node)
	return distmap

def dijkstra_grid(start_pos, width, height, available, target=None, diags=False, heuristic=None):
	if diags:
		steps = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
	else:
		steps = [(-1, 0), (0, -1), (0, 1), (1, 0)]
	def neighbours(pos):
		x, y = pos
		for dx, dy in steps:
			if 0 <= x + dx < width and 0 <= y + dy < height and available(x + dx, y + dy):
				yield (x + dx, y + dy), 1
	return dijkstra(start_pos, neighbours, target, heuristic)
