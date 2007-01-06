# $Id: Makefile,v 1.1 2005/05/12 16:41:38 hxml Exp $

HXT_HOME	= ../../..
PKGFLAGS	= 
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

DIST		= $(HXT_HOME)/dist/examples/arrows
DIST_DIR	= $(DIST)/absurls

prog	= AbsURIs

all	: $(prog)

AbsURIs	: AbsURIs.hs ProcessDocument.hs
	$(GHC) --make -o $@ $<

force		:
	$(GHC) --make -o $(prog) $(prog).hs

test		: $(prog)
		@echo "===> run a few simple test cases"
		$(MAKE) test0

EX		= ./lousy.html

test0		:
		@echo "===> the source of a lousy html document" ; echo ; sleep 2
		cat $(EX)
		@sleep 2 ; echo ; echo "===> all refs (href, src attributes) are transformed into absolute URIs with respect to the base element" ; echo ; sleep 2
		$(prog) --trace=0 --encoding=ISO-8859-1 --output-encoding=ISO-8859-1 --indent --do-not-issue-warnings $(EX)
		@echo

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp $(EX) Makefile $(prog).hs ProcessDocument.hs $(DIST_DIR)

clean		:
		rm -f $(prog) *.o *.hi
