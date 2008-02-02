# $Id: Makefile,v 1.9 2006/11/11 15:36:03 hxml Exp $

HXT_HOME	= ../../..
PKGFLAGS	= 
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

DIST		= $(HXT_HOME)/dist/examples/arrows
DIST_DIR	= $(DIST)/performance

prog		= ./GenDoc
prog2		= ./ReadDoc
prog3		= ./PruneRight
prog4		= ./PruneLeft

all		: $(prog) $(prog2) $(prog3) $(prog4)

$(prog)		: $(prog).hs
		$(GHC) --make -o $@ $<

local		:
		ghc --make -o $(prog) -Wall -O -fglasgow-exts -ignore-package hxt -i../../../src $(prog).hs

$(prog2)	: $(prog)
		ln -f $(prog) $(prog2)

$(prog3)	: $(prog)
		ln -f $(prog) $(prog3)

$(prog4)	: $(prog)
		ln -f $(prog) $(prog4)

# generate and read documents containing a binary tree
# with 2^i leaf nodes containing the numbers 1 to 2^i
# for i up to at least 22 (8M XML elements) output works fine
# for i up to 19 (1M XML elements) input works without swapping
# with i=20 swapping starts, but the program it still terminates
# the size of the XML file for i=20 is about 36Mb
# these tests have run on a box with 1Gb memory

tests		= 2 3 10 11 12 13 14 15 16 17 18 19

test		: $(prog)
		$(MAKE) genfiles
		$(MAKE) readfiles
		$(MAKE) pruneright
		$(MAKE) pruneleft

genfiles	:
		@for i in $(tests) ; \
		do \
		echo time $(prog) $$i ; \
		time $(prog) $$i ; \
		ls -l tree-*$$i.xml ; \
		echo ; \
		done

readfiles	:
		@for i in $(tests) ; \
		do \
		echo time $(prog2) $$i ; \
		time $(prog2) $$i ; \
		echo ; \
		done

pruneright	:
		@for i in $(tests) ; \
		do \
		echo time $(prog3) $$i ; \
		time $(prog3) $$i ; \
		echo ; \
		done

pruneleft	:
		@for i in $(tests) ; \
		do \
		echo time $(prog4) $$i ; \
		time $(prog4) $$i ; \
		echo ; \
		done

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp Makefile GenDoc.hs $(DIST_DIR)

clean		:
		rm -f $(prog) *.o *.hi *.xml

