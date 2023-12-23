#!/usr/bin/python3
dat1="""\
nsz{x>1625:R,R}
ldx{s>3581:R,s<3393:A,x<1640:A,A}
pz{x<645:A,A}
[...snip...]"""

#dat1 = "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}"
#dat2 = "{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}"

def parsedat1(s):
	tag, bits = s.rstrip("}").split("{")
	bits = bits.split(",")
	last = bits[-1]
	rest = bits[:-1]
	rules = []
	for i in rest:
		a, b = i.split(":")
		rules.append((a[0], a[1], int(a[2:]), b))
	return tag, (rules, last)

def parsedat2(s):
	return {i.split("=")[0]:int(i.split("=")[1]) for i in s.lstrip("{").rstrip("}").split(",")}

dat1 = [parsedat1(i) for i in dat1.split("\n")]
dat1 = dict(dat1)
dat2 = [parsedat2(i) for i in dat2.split("\n")]

def process(part):
	flow = 'in'
	while flow not in ["A", "R"]:
		rule, last = dat1[flow]
		for a, b, v, n in rule:
			if (b == '<' and part[a] < v) or (b == '>' and part[a] > v):
				flow = n
				break
		else:
			flow = last
	return flow
print(sum(j for i in dat2 if process(i) == 'A' for j in i.values()))

def part2():
	todo = [({k: (1, 4001) for k in "xmas"}, "in", 0)]
	count = 0
	while todo:
		ranges, flow, ruleix = todo.pop(0)

		if flow == 'A':
			count += (ranges["x"][1] - ranges["x"][0]) * (ranges["m"][1] - ranges["m"][0]) * (ranges["a"][1] - ranges["a"][0]) * (ranges["s"][1] - ranges["s"][0])
			continue
		elif flow == 'R':
			continue

		if ruleix >= len(dat1[flow][0]):
			todo.append((ranges, dat1[flow][1], 0))
		else:
			a, b, v, n = dat1[flow][0][ruleix]
			if b == '<' and ranges[a][1] <= v:
				todo.append((ranges, n, 0))
			elif b == '>' and ranges[a][0] > v:
				todo.append((ranges, n, 0))
			else:
				if b == '<':
					subrange1 = dict(ranges)
					subrange1[a] = (ranges[a][0], v)
					subrange2 = dict(ranges)
					subrange2[a] = (v, ranges[a][1])
					todo.append((subrange1, n, 0))
					todo.append((subrange2, flow, ruleix + 1))
				else:
					subrange1 = dict(ranges)
					subrange1[a] = (ranges[a][0], v+1)
					subrange2 = dict(ranges)
					subrange2[a] = (v+1, ranges[a][1])
					todo.append((subrange1, flow, ruleix + 1))
					todo.append((subrange2, n, 0))

	return count
print(part2())
