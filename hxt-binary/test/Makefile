PROG	= TestXmlTreeBinary

all	: $(PROG)

$(PROG)	: $(PROG).hs
	ghc --make -i../src -o $(PROG) $(PROG).hs

test	: $(PROG)
	./$(PROG)

clean	:
	rm -f *.o *.hi t.bin t.bin.bz $(PROG)

 