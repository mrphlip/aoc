#!/usr/bin/python3
a = open("01.txt").read()
a = [[int(j) for j in i.split("\n") if j] for i in a.split("\n\n")]
print(max(sum(i) for i in a))
a.sort(key=sum)
print(sum(j for i in a[-3:] for j in i))
