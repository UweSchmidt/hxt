PROG	= TestXmlCache

all	: $(PROG)

$(PROG)	: $(PROG).hs
	ghc --make -i../src -o $(PROG) $(PROG).hs

test	: $(PROG)
	@echo $$(date): reading and caching "www.haskell.org"
	./$(PROG)
	find cache -type f | xargs ls --full-time
	@echo $$(date): reading the same page from cache
	./$(PROG)
	find cache -type f | xargs ls --full-time
	@echo sleeping 12 second
	@sleep 12
	@echo $$(date): cache is valid only 10 seconds, no cache hit
	./$(PROG)
	find cache -type f | xargs ls --full-time


clean	:
	rm -f *.o *.hi haskell.org.html $(PROG)
	rm -rf ./cache
