all: 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 # 16 17 18 19 20 21 22 23 25

clean:
	rm -f [0-9][0-9] *.hi *.o ../*.hi ../*.o

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
#16: 16.hs ../Utils.hs
#17: 17.hs ../Utils.hs
#18: 18.hs ../Utils.hs
#19: 19.hs ../Utils.hs
#20: 20.hs ../Utils.hs
#21: 21.hs ../Utils.hs
#22: 22.hs ../Utils.hs
#23: 23.hs ../Utils.hs
#24: 24.hs ../Utils.hs
#25: 25.hs ../Utils.hs
