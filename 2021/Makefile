all: 01 02

clean:
	rm -f [0-9][0-9] *.hi *.o ../*.hi ../*.o

.PHONY: all clean

%: %.hs
	ghc -O2 -threaded -with-rtsopts="-N" -o $@ $^

01: 01.hs
02: 02.hs
