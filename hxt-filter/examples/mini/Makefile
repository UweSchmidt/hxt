# $Id: Makefile,v 1.5 2005/04/14 12:52:52 hxml Exp $

HXT_HOME	= ../..
PKGFLAGS	=
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

prog		= ./ParseExample

all		: $(prog)

force		:
		$(GHC) --make -o $(prog) $(prog).hs

$(prog)		: $(prog).hs
		$(GHC) --make -o $@ $<

test		: $(prog)
		@echo "===> a very simple test"
		@echo "===> a xml source"
		@cat example1.xml
		@sleep 2; echo "===> parse, validate and normalise the document"
		$(prog) example1.xml

DIST		= $(HXT_HOME)/dist/examples
DIST_DIR	= $(DIST)/mini

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp example1.xml Makefile $(prog).hs $(DIST_DIR)

clean		:
		rm -f $(prog) *.o *.hi

