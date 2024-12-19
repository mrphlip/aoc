__all__ = ["PrioQueue", "QueueEmpty"]

import heapq

# Based on sample code in the Python stdlib docs
# https://docs.python.org/3/library/heapq.html#priority-queue-implementation-notes

_REMOVED = object()

class QueueEmpty(Exception):
	pass

class PrioQueue:
	def __init__(self):
		self._heap = []
		self._entries = {}
		self._counter = 0

	def add(self, key, priority, value=None):
		# Are we updating a key that's already in the heap?
		if key in self._entries:
			if self._entries[key][0] == priority:
				# If the priority is unchanged, just update the value and leave
				self._entries[key][3] = value
				return
			else:
				# If the priority is changing, need to remove the old entry and re-add it
				self.remove(key)

		entry = [priority, self._counter, key, value]
		self._counter += 1
		self._entries[key] = entry
		heapq.heappush(self._heap, entry)

	def remove(self, key):
		entry = self._entries.pop(key)
		entry[2] = _REMOVED

	def pop(self):
		while self._heap:
			priority, ix, key, value = heapq.heappop(self._heap)
			if key is not _REMOVED:
				del self._entries[key]
				return key, priority, value
		raise QueueEmpty("empty queue")

	def __bool__(self):
		return bool(self._entries)

	def __getitem__(self, key):
		priority, ix, key, value = self._entries[key]
		return priority, value

	def get(self, key, default=(None, None)):
		try:
			return self[key]
		except KeyError:
			return default

	def __setitem__(self, key, val):
		if isinstance(val, (tuple, list)):
			self.add(key, *val)
		else:
			self.add(key, val)

	def __contains__(self, key):
		return key in self._entries
