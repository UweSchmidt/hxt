# $Id: Makefile,v 1.9 2006/11/11 15:36:03 hxml Exp $

HXT_HOME	= ../../..
PKGFLAGS	= 
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

DIST		= $(HXT_HOME)/dist/examples/arrows
DIST_DIR	= $(DIST)/performance

CNT		= 3

ropts		= +RTS -s -RTS 
popts		= +RTS -P -hy -RTS

prog		= ./GenDoc
prog2		= ./ReadDoc
prog3		= ./PruneRight
prog4		= ./PruneLeft
prog5		= ./MemTest
progs		= $(prog) $(prog2) $(prog3) $(prog4) $(prog5)

all		: $(progs)

prof		:
		$(MAKE) all PKGFLAGS="-prof -auto-all -caf-all -rtsopts"

$(prog)		: $(prog).hs
		$(GHC) --make -o $@ $<

local		:
		$(GHC) --make -o $(prog) -fglasgow-exts -ignore-package hxt -i../../../src $(prog).hs

$(prog2)	: $(prog)
		ln -f $(prog) $(prog2)

$(prog3)	: $(prog)
		ln -f $(prog) $(prog3)

$(prog4)	: $(prog)
		ln -f $(prog) $(prog4)

$(prog5)	: $(prog)
		ln -f $(prog) $(prog5)

# generate and read documents containing a binary tree
# with 2^i leaf nodes containing the numbers 1 to 2^i
# for i up to at least 22 (8M XML elements) output works fine
# for i up to 19 (1M XML elements) input works without swapping
# with i=20 swapping starts, but the program it still terminates
# the size of the XML file for i=20 is about 36Mb
# these tests have run on a box with 1Gb memory

tests		= 2 3 10 11 12

test		: $(prog)
		$(MAKE) genfiles   tests="$(tests)"
		$(MAKE) readfiles  tests="$(tests)"
		$(MAKE) pruneright tests="$(tests)"
		$(MAKE) pruneleft  tests="$(tests)"

perftest	: $(prog)
		$(MAKE) test tests="2 3 10 11 12 13 14 15 16 17 18 19 20"

pgenfiles	:
		rm -f $(prog).aux $(prog).hp $(prog).ps $(prog).prof
		$(MAKE) genfiles ropts="$(popts)" tests=18
		hp2ps -c $(prog).hp

preadfiles	:
		$(MAKE) readfiles ropts="$(popts)" tests=18
		hp2ps -c $(prog2).hp

genfiles	:
		@for i in $(tests) ; \
		do \
		echo time $(prog) $(ropts) $$i ; \
		time $(prog) $$i ; \
		ls -l tree-*$$i.xml ; \
		echo ; \
		done

readfiles	:
		@for i in $(tests) ; \
		do \
		echo time $(prog2) $(ropts) $$i ; \
		time $(prog2) $$i ; \
		echo ; \
		done

pruneright	:
		@for i in $(tests) ; \
		do \
		echo time $(prog3) $(ropts) $$i ; \
		time $(prog3) $$i ; \
		echo ; \
		done

pruneleft	:
		@for i in $(tests) ; \
		do \
		echo time $(prog4) $(ropts) $$i ; \
		time $(prog4) $$i ; \
		echo ; \
		done

memtest	:
		@for i in $(tests) ; \
		do \
		echo time $(prog5) $$i ; \
		time $(prog5) $(ropts) $$i ; \
		echo ; \
		done

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp Makefile GenDoc.hs $(DIST_DIR)

clean		:
		rm -f $(progs) *.o *.hi *.xml

