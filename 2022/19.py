#!/usr/bin/python3
dat = [
	(4, 4, 4, 7 , 2, 19),(2, 4, 4, 20, 4, 18),[...snip...]
]
#dat = [(4, 2, 3, 14, 2, 7), (2, 3, 3, 8, 3, 12)]

def buildtime(cost, available, bots):
	if available >= cost:
		return 1
	else:
		return ((cost - available) + bots-1) // bots + 1

def maximise(bp, time):
	orebot_cost, claybot_cost, obsbot_cost_ore, obsbot_cost_clay, geodebot_cost_ore, geodebot_cost_obs = bp
	maxorebots = max(orebot_cost, claybot_cost, obsbot_cost_ore, geodebot_cost_ore)
	def walk(n, ore, clay, obs, geode, orebot, claybot, obsbot, geodebot):
		yield geode + n * geodebot

		if orebot < maxorebots:
			bt = buildtime(orebot_cost, ore, orebot)
			if n >= bt:
				yield from walk(n - bt, ore + bt*orebot - orebot_cost, clay + bt*claybot, obs + bt*obsbot, geode + bt*geodebot, orebot + 1, claybot, obsbot, geodebot)

		if claybot < obsbot_cost_clay:
			bt = buildtime(claybot_cost, ore, orebot)
			if n >= bt:
				yield from walk(n - bt, ore + bt*orebot - claybot_cost, clay + bt*claybot, obs + bt*obsbot, geode + bt*geodebot, orebot, claybot + 1, obsbot, geodebot)

		if claybot > 0 and obsbot < geodebot_cost_obs:
			bt = max(buildtime(obsbot_cost_ore, ore, orebot), buildtime(obsbot_cost_clay, clay, claybot))
			if n >= bt:
				yield from walk(n - bt, ore + bt*orebot - obsbot_cost_ore, clay + bt*claybot - obsbot_cost_clay, obs + bt*obsbot, geode + bt*geodebot, orebot, claybot, obsbot + 1, geodebot)

		if obsbot > 0:
			bt = max(buildtime(geodebot_cost_ore, ore, orebot), buildtime(geodebot_cost_obs, obs, obsbot))
			if n >= bt:
				yield from walk(n - bt, ore + bt*orebot - geodebot_cost_ore, clay + bt*claybot, obs + bt*obsbot - geodebot_cost_obs, geode + bt*geodebot, orebot, claybot, obsbot, geodebot + 1)

	return max(walk(time, 0, 0, 0, 0, 1, 0, 0, 0))

tot = 0
for i, bp in enumerate(dat, 1):
	res = maximise(bp, 24)
	print(f"  {i}: {res}")
	tot += i * res
print(tot)

prod = 1
for i, bp in enumerate(dat[:3], 1):
	res = maximise(bp, 32)
	print(f"  {i}: {res}")
	prod *= res
print(prod)
