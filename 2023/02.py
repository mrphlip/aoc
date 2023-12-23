#!/usr/bin/python
dat = [
	[(2 ,"blue", 3 ,"red"),( 3 ,"green", 3 ,"blue", 6 ,"red"),( 4 ,"blue", 6 ,"red"),( 2 ,"green", 2 ,"blue", 9 ,"red"),( 2 ,"red", 4 ,"blue")],
	[(4 ,"red", 1 ,"green"),( 3 ,"red"),( 13 ,"green", 5 ,"red", 3 ,"blue"),( 3 ,"green", 2 ,"red"),( 3 ,"blue", 5 ,"red", 3 ,"green"),( 2 ,"red", 3 ,"blue", 12 ,"green")],
	[...snip...]
]

#dat = [[(3 ,"blue", 4 ,"red"),( 1 ,"red", 2 ,"green", 6 ,"blue"),( 2 ,"green")], [(1 ,"blue", 2 ,"green"),( 3 ,"green", 4 ,"blue", 1 ,"red"),( 1 ,"green", 1 ,"blue")], [(8 ,"green", 6 ,"blue", 20 ,"red"),( 5 ,"blue", 4 ,"red", 13 ,"green"),( 5 ,"green", 1 ,"red")], [(1 ,"green", 3 ,"red", 6 ,"blue"),( 3 ,"green", 6 ,"red"),( 3 ,"green", 15 ,"blue", 14 ,"red")], [(6 ,"red", 1 ,"blue", 3 ,"green"),( 2 ,"blue", 1 ,"red", 2 ,"green")], ]

cap = {"red":12,"green":13,"blue":14}
def possible(game):
	for step in game:
		for i in range(0,len(step),2):
			if step[i] > cap.get(step[i+1],0):
				return False
	return True

print(sum(i for i, g in enumerate(dat, 1) if possible(g)))

def power(game):
	req = {"red":0,"green":0,"blue":0}
	for step in game:
		for i in range(0,len(step),2):
			if step[i] > req[step[i+1]]:
				req[step[i+1]] = step[i]
	return req["red"] * req["green"] * req["blue"]
print(sum(power(g) for g in dat))
