HXT_HOME	= ../..
PKGFLAGS	= 
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

prog		= ./HXmlParser

all		: $(prog)

$(prog)		: HXmlParser.hs
		$(GHC) --make -o $@ $<

force		:
		$(GHC) --make -o $(prog) $(prog).hs

test		: $(prog)
		@echo "===> run a few simple test cases with the validating parser"
		@echo "===> first see all command line options"
		$(prog) --help
		@echo
		$(MAKE) xslttest

xslttest	:
		$(MAKE) -C xslt test


clean		:
		rm -f $(prog) *.o *.hi

.PHONY		: all force test xslttest clean
