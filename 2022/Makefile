all: 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 17 18 21a 21b

clean:
	rm -f [0-9][0-9] 21a 21b *.hi *.o ../*.hi ../*.o

.PHONY: all clean

%: %.hs
	ghc -O2 -threaded -with-rtsopts="-N" -o $@ $^

01: 01.hs
02: 02.hs
03: 03.hs ../Utils.hs
04: 04.hs ../Utils.hs
05: 05.hs ../Utils.hs
06: 06.hs ../Utils.hs
07: 07.hs
08: 08.hs ../Utils.hs ../Direction.hs
09: 09.hs ../Direction.hs
10: 10.hs ../Utils.hs
11: 11.hs ../Utils.hs
12: 12.hs ../Utils.hs ../Dijkstra.hs ../Direction.hs
13: 13.hs ../Utils.hs
14: 14.hs ../Utils.hs
15: 15.hs ../Utils.hs ../Range.hs
# Not gonna do 16 in Haskell
17: 17.hs ../Utils.hs ../Direction.hs
18: 18.hs
21a: 21a.hs
21b: 21b.hs
