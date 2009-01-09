# $Id: Makefile,v 1.9 2006/11/11 15:36:03 hxml Exp $

HXT_HOME	= ../../..
PKGFLAGS	= 
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

DIST		= $(HXT_HOME)/dist/examples/arrows
DIST_DIR	= $(DIST)/RegexXMLSchema

CNT		= 3

ropts		= +RTS -s -RTS 

prog		= ./REtest
prog2		= ./Lines
prog3		= ./RElines
prog4		= ./Words
prog5		= ./REwords
prog0		= ./Copy

progs		= $(prog) $(prog0) $(prog2) $(prog3) $(prog4) $(prog5)

all		: $(progs)

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

$(prog0)	: $(prog)
		ln -f $(prog) $(prog0)

# generate and read documents containing a binary tree
# with 2^i leaf nodes containing the numbers 1 to 2^i
# for i up to at least 22 (8M XML elements) output works fine
# for i up to 19 (1M XML elements) input works without swapping
# with i=20 swapping starts, but the program it still terminates
# the size of the XML file for i=20 is about 36Mb
# these tests have run on a box with 1Gb memory

tests		= 18

test		: $(prog)
		$(MAKE) genfiles   tests="$(tests)"
		$(MAKE) copy       tests="$(tests)"
		$(MAKE) lines      tests="$(tests)"
		$(MAKE) relines    tests="$(tests)"
		$(MAKE) words      tests="$(tests)"
		$(MAKE) rewords    tests="$(tests)"

perftest	: $(prog)
		$(MAKE) test tests="2 3 10 11 12 13 14 15 16 17 18 19 20"

genfiles	:
		@for i in $(tests) ; \
		do \
		echo time $(prog) $(ropts) $$i ; \
		time $(prog) $(ropts) $$i ; \
		ls -l tree-*$$i.xml ; \
		echo ; \
		done

copy	:
		@for i in $(tests) ; \
		do \
		echo time $(prog0) $(ropts) $$i ; \
		time $(prog0) $(ropts) $$i ; \
		ls -l tree-*$$i.xml.copy ; \
		echo ; \
		done

lines	:
		@for i in $(tests) ; \
		do \
		echo time $(prog2) $(ropts) $$i ; \
		time $(prog2) $(ropts) $$i ; \
		ls -l tree-*$$i.xml.lines ; \
		echo ; \
		done

relines	:
		@for i in $(tests) ; \
		do \
		echo time $(prog3) $(ropts) $$i ; \
		time $(prog3) $(ropts) $$i ; \
		ls -l tree-*$$i.xml.relines ; \
		echo ; \
		done

words	:
		@for i in $(tests) ; \
		do \
		echo time $(prog4) $(ropts) $$i ; \
		time $(prog4) $(ropts) $$i ; \
		ls -l tree-*$$i.xml.words ; \
		echo ; \
		done

rewords	:
		@for i in $(tests) ; \
		do \
		echo time $(prog5) $$i ; \
		time $(prog5) $(ropts) $$i ; \
		ls -l tree-*$$i.xml.rewords ; \
		echo ; \
		done

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp Makefile REtest.hs $(DIST_DIR)

clean		:
		rm -f $(progs) *.o *.hi *.xml *.xml.*

