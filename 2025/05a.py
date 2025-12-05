#!/usr/bin/python
from aocimports import *

dat = [
	(169486974574545,170251643963353),
	(350457710225863,350888576149828),
	[...]
]
dat2 = [
	166774327825644,
	91047458369966,
	[...]
]

#dat = [(3,5),(10,14),(16,20),(12,18)]; dat2 = [1,5,8,11,17,32]

dat = [(i,j+1) for i,j in dat]

ranges = Ranges(dat)
print(len([i for i in dat2 if i in ranges]))
print(len(ranges))
