# $Id: Makefile,v 1.2 2005/09/30 14:41:44 hxml Exp $

HXT_HOME	= ../../..
PKGFLAGS	= 
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

DIST		= $(HXT_HOME)/dist/examples/arrows
DIST_DIR	= $(DIST)/RelaxTestSuite

prog		= ./RunTestCases
modules		= RunTestCases.hs ReadDir.hs TestCases.hs

all		: $(prog)

prof		:
		ghc --make -o $(prog) -Wall -prof -auto-all -O -fglasgow-exts -ignore-package hxt -ignore-package HTTP -i../../../src $(prog).hs

local		:
		ghc --make -o $(prog) $(GHCFLAGS) -fglasgow-exts -ignore-package hxt -i../../../src $(prog).hs

$(prog)		: $(modules)
		$(GHC) --make -o $@ $@.hs

force		:
		$(GHC) --make -o $(prog) $(prog).hs

testcases	= testCases
testarchive	= RelaxTestCases.tgz

test		:
		$(MAKE) testrng
		$(MAKE) testschema

testrng		: $(prog) $(testcases)
		$(prog) $(shell pwd)/$(testcases)

testschema	:
		$(prog) $(shell pwd)/w3cSchemaDataTypes

$(testcases)	: $(testarchive)
		tar xvzf $(testarchive)
		touch $(testcases)

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp Makefile $(modules) $(testarchive) $(DIST_DIR)

clean		:
		rm -f $(prog) *.o *.hi

# eof ------------------------------------------------------------
