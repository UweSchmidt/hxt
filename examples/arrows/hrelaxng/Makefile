# $Id: Makefile,v 1.2 2005/09/30 14:41:44 hxml Exp $

HXT_HOME	= ../../..
PKGFLAGS	= 
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

DIST		= $(HXT_HOME)/dist/examples/arrows
DIST_DIR	= $(DIST)/hrelaxng

prog		= ./HRelaxNG

all		: $(prog)

prof		:
		ghc --make -o $(prog) -Wall -prof -auto-all -O -fglasgow-exts -ignore-package hxt -ignore-package HTTP -i../../../src $(prog).hs

local		:
		ghc --make -o $(prog) $(GHCFLAGS) -fglasgow-exts -ignore-package hxt -i../../../src $(prog).hs

$(prog)		: $(prog).hs
		$(GHC) --make -o $@ $<

force		:
		$(GHC) --make -o $(prog) $(prog).hs

test		: $(prog)
		@echo "===> Relax NG examples" ; echo ; sleep 1
		@$(MAKE) example1 example1a example2 example3 example5

example1	:
		@echo "===> Validate a valid Relax NG schema with respect to a valid xml file" ; echo ; sleep 1
		$(prog) ./valid1.xml ./valid1.rng
		@echo

example1a	:
		@echo "a valid document"
		$(prog) ./valid2.xml ./valid2.rng
		@echo

example1b	:
		@echo "===> 1. invalid document\n"
		$(prog) ./inv2.xml ./valid2.rng || true
		@echo "\n===> 2. invalid document\n"
		$(prog) ./inv3.xml ./valid2.rng || true
		@echo

example2	:
		@echo "===> Validate a valid Relax NG schema with respect to a invalid xml file" ; echo ; sleep 1
		$(prog) ./invalid1.xml ./valid1.rng || true
		@echo

example3	:
		@echo "===> Incorrect Relax NG schema" ; echo ; sleep 1
		$(prog) ./valid1.xml ./invalid2.rng || true
		@echo

example4	:
		@echo "===> Same example with \"--output-pattern-transformations\" option " ; echo ; sleep 1
		$(prog) --output-pattern-transformations  ./valid1.xml ./invalid2.rng || true
		@echo

example5	:
		@echo "===> Incorrect Relax NG schema with two errors" ; echo ; sleep 1
		$(prog) ./valid1.xml ./invalid3.rng || true
		@echo

EX		= $(wildcard *valid*.xml *valid*.rng)

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp $(EX) Makefile $(prog).hs $(DIST_DIR)

clean		:
		rm -f $(prog) *.o *.hi

# eof ------------------------------------------------------------
