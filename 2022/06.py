#!/usr/bin/python3
a = "qfmfhmhjmjggw[...snip...]"
for i in range(len(a)):
	if len(set(a[i:i+4])) == 4:
		break
print(i+4)
for i in range(len(a)):
	if len(set(a[i:i+14])) == 14:
		break
print(i+14)
