all: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24

clean:
	rm -f [0-9] [0-9][0-9] *.hi *.o

.PHONY: all clean

%: %.hs
	ghc -threaded -with-rtsopts="-N" -o $@ $^

1: 1.hs
2: 2.hs Utils.hs Intcode.hs
3: 3.hs Utils.hs
4: 4.hs
5: 5.hs Utils.hs Intcode.hs
6: 6.hs
7: 7.hs Utils.hs Intcode.hs
8: 8.hs Utils.hs
9: 9.hs Utils.hs Intcode.hs
10: 10.hs Utils.hs
11: 11.hs Utils.hs Intcode.hs
12: 12.hs Utils.hs
13: 13.hs Utils.hs Intcode.hs
14: 14.hs Utils.hs
15: 15.hs Utils.hs Intcode.hs Direction.hs Dijkstra.hs
16: 16.hs Utils.hs
17: 17.hs Utils.hs Intcode.hs Direction.hs
18: 18.hs Utils.hs Direction.hs Dijkstra.hs
19: 19.hs Utils.hs Intcode.hs Direction.hs
20: 20.hs Utils.hs Direction.hs Dijkstra.hs
21: 21.hs Utils.hs Intcode.hs
22: 22.hs Utils.hs
23: 23.hs Utils.hs Intcode.hs
24: 24.hs Utils.hs
