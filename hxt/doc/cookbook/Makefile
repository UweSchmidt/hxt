GHCFLAGS	= -Wall -O2
GHCPACKAGES	= -package hxt
GHC 		= ghc $(GHCFLAGS)
HDOC_DIR	= doc/hdoc
HDOC_FLAGS	=
RDF_SRC 	= $(wildcard ./*.hs) 
RDF_LIB		= $(wildcard ./*.o) \
			  $(wildcard ./*.hi)

HDOCFLAGS	= -o $(HDOC_DIR) -h -t "RDF/XML parser and SPARQL parser"
HDOC		= haddock $(HDOCFLAGS)

prog		= HRdf 

all		: $(RDF_SRC)
		$(GHC) --make $(GHCPACKAGES) -o $(prog) $< 

doc 		: $(RDF_SRC)
		[ -d $(HDOC_DIR) ] || mkdir -p $(HDOC_DIR)
		$(HDOC) $(HDOC_FLAGS) $(RDF_SRC)

clean 		: $(RDF_LIB)
		rm -f $(RDF_LIB)	
