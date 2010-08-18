prog	= ./main

all	: $(prog)

$(prog)	: Main.hs
	ghc --make -Wall -O2 -o $@ $<

test	: $(prog)
	$(prog)

clean	:
	rm -f $(prog) *.hi *.o

.PHONY	: all test clean
