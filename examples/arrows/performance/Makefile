# $Id: Makefile,v 1.9 2006/11/11 15:36:03 hxml Exp $

HXT_HOME	= ../../..
PKGFLAGS	= 
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

DIST		= $(HXT_HOME)/dist/examples/arrows
DIST_DIR	= $(DIST)/performance

prog		= ./GenDoc
prog2		= ./ReadDoc

all		: $(prog) $(prog2)

$(prog)		: GenDoc.hs
		$(GHC) --make -o $@ $<

$(prog2)	: $(prog)
		ln -f $(prog) $(prog2)

tests		= 2 3 10 11 12 13 14 15 16 17 18 19 20 21 22

test		: $(prog)
		$(MAKE) genfiles
		$(MAKE) readfiles

genfiles	:
		@for i in $(tests) ; \
		do \
		echo time $(prog) $$i ; \
		time $(prog) $$i ; \
		ls -l "tree-$$i.xml" ; \
		echo ; \
		done

readfiles	:
		@for i in $(tests) ; \
		do \
		echo time $(prog2) $$i ; \
		time $(prog2) $$i ; \
		echo ; \
		done

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp Makefile GenDoc.hs $(DIST_DIR)

clean		:
		rm -f $(prog) *.o *.hi *.xml

