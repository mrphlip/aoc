from collections import defaultdict

p = [5, 10]
score = [0,0]
turn = 0

state = 0
rollcount = 0
def det_roll():
	global state, rollcount
	rollcount += 1
	state += 1
	if state > 100:
		state -= 100
	return state
while True:
	roll = det_roll() + det_roll() + det_roll()
	p[turn] = (p[turn] + roll - 1) % 10 + 1
	score[turn] += p[turn]
	if score[turn] >= 1000:
		break
	turn = 1 - turn
print(score[1-turn] * rollcount)


p = [5,10]
#p = [4,8]
todo_states = defaultdict(int)
todo_states[tuple(p),(0,0),0] = 1
win_universes = [0,0]
while todo_states:
	pos, score, turn = min(todo_states.keys(), key=lambda x:x[1][0]+x[1][1])
	count = todo_states.pop((pos, score, turn))
	for i in range(1,4):
		for j in range(1,4):
			for k in range(1,4):
				newpos = list(pos)
				newpos[turn] = (newpos[turn] + i + j + k - 1) % 10 + 1
				newscore = list(score)
				newscore[turn] += newpos[turn]
				if newscore[turn] >= 21:
					win_universes[turn] += count
				else:
					todo_states[tuple(newpos), tuple(newscore), 1-turn] += count
#print(win_universes)
print(max(win_universes))
