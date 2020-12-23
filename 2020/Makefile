all: 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 # 23 24 25

clean:
	rm -f [0-9][0-9] *.hi *.o ../*.hi ../*.o

.PHONY: all clean

%: %.hs
	ghc -O2 -threaded -with-rtsopts="-N" -o $@ $^

01: 01.hs
02: 02.hs ../Utils.hs
03: 03.hs
04: 04.hs
05: 05.hs ../Utils.hs
06: 06.hs
07: 07.hs ../Utils.hs
08: 08.hs ../Utils.hs
09: 09.hs ../Utils.hs
10: 10.hs
11: 11.hs ../Utils.hs
12: 12.hs ../Vector.hs
13: 13.hs ../Utils.hs
14: 14.hs ../Utils.hs
15: 15.hs
16: 16.hs ../Utils.hs ../Range.hs
17: 17.hs ../Utils.hs
18: 18.hs
19: 19.hs ../Utils.hs
20: 20.hs ../Utils.hs ../Direction.hs
21: 21.hs ../Utils.hs
22: 22.hs
23: 23.cpp
	g++ -O3 -o $@ $^
#24: 24.hs
#25: 25.hs

18.hs: 18.sh 18.txt
	./18.sh
