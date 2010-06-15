# $Id: Makefile,v 1.7 2005/04/14 12:52:51 hxml Exp $

HXT_HOME	= ../..
PKGFLAGS	=
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

prog		= ./FilterExample
opts		= --parse-html --do-not-issue-warnings
runprog		= $(prog) $(opts)

all		: $(prog)

force		:
		$(GHC) --make -o $(prog) $(prog).hs

$(prog)		: $(prog).hs
		$(GHC) --make -o $@ $<

EX		= ./lousy.html

test		: $(prog)
		@echo "===> Filter examples"
		@echo ; echo "===> the filter parses a document and selects or modifies parts of it"
		@echo ; echo "===> usage" ; echo
		./FilterExample --help
		@echo ; sleep 2
		@$(MAKE) test1 test2 test3 test4 test5

test1		:
		@sleep 2 ; echo ; echo "===> extract all a tags with a href attribute" ; echo ; sleep 2
		$(runprog) --filter=all-a-tags-with-href-attr $(EX)
		@echo

test2		:
		@sleep 2 ; echo ; echo "===> extract all href attribute values for a tags" ; echo ; sleep 2
		$(runprog) --filter=all-href-values-of-a-tags $(EX)
		@echo

test3		:
		@sleep 2 ; echo ; echo "===> change all src attribute values of img tags to absolut uris" ; echo ; sleep 2
		$(runprog) --filter=change-img-src-to-abs-uri $(EX)
		@echo

test4		:
		@sleep 2 ; echo ; echo "===> select all text" ; echo ; sleep 2
		$(runprog) --filter=all-text $(EX)
		@echo

test5		:
		@sleep 2 ; echo ; echo "===> select all text within body, but remove whitespace" ; echo ; sleep 2
		$(runprog) --filter=all-body-text-without-whitespace $(EX)
		@echo

DIST		= $(HXT_HOME)/dist/examples
DIST_DIR	= $(DIST)/hfilter

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp $(EX) Makefile $(prog).hs $(DIST_DIR)

clean		:
		rm -f $(prog) *.o *.hi
