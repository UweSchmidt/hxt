# $Id: Makefile,v 1.9 2006/11/11 15:36:03 hxml Exp $

HXT_HOME	= ../../..
PKGFLAGS	=
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

DIST		= $(HXT_HOME)/dist/examples/arrows
DIST_DIR	= $(DIST)/hparser

prog		= ./HXmlParser

all		: $(prog)

prof		:
		ghc --make -o $(prog) -Wall -prof -auto-all -O -fglasgow-exts -ignore-package hxt -ignore-package HTTP -i../../../src $(prog).hs

local		:
		ghc --make -o $(prog) $(GHCFLAGS) -fglasgow-exts -ignore-package hxt -i../../../src $(prog).hs

$(prog)		: HXmlParser.hs
		$(GHC) --make -o $@ $<

force		:
		$(GHC) --make -o $(prog) $(prog).hs

test		: $(prog)
		@echo "===> run a few simple test cases with the validating parser"
		@echo "===> first see all command line options"
		$(prog) --help
		@echo
		$(MAKE) test0 test1 test2 test3 test4

EX1		= ./example1.xml
EXi		= ./invalid.xml
EX2		= ./xhtml.xml
EX3		= ./namespace0.xml
EX3a		= ./namespace1.xml
EX4		= ./lousy.html
EX4a		= ./emptyElements.html

EX		= $(wildcard example*.xml) $(wildcard lousy*.html) $(wildcard empty*.html) $(wildcard *valid*.xml *valid*.rng) $(wildcard namespace*.xml)

test0		:
		@echo "===> a 1. simple valid document"
		$(prog) $(EX1)
		@echo
		@echo "===> the dom tree of the same document (without any redundant whitespace)"
		$(prog) --show-tree --remove-whitespace $(EX1)
		@echo
		@echo "===> the next test case contains validation erors, but it does not fail, no validation with expat"
		$(prog) --verbose $(EXi) || true
		@echo

test1		:
		@echo "===> the source of a very simple valid document" ; echo ; sleep 2
		cat $(EX1)
		@sleep 2 ; echo ; echo "===> parser will emit UTF-8" ; echo ; sleep 2
		$(prog) --output-encoding=UTF-8 $(EX1)

test2		:
		@echo "===> the source of a xhtml document" ; echo ; sleep 2
		cat $(EX2)
		@echo "that document has" `cat $(EX2) | wc -l` "lines"
		@sleep 2 ; echo ; echo "===> parser will try to indent the output" ; echo ; sleep 2
		$(prog) --indent $(EX2)
		@sleep 2 ; echo ; echo "===> once again, but remove all markup" ; echo ; sleep 2
		$(prog) --action=only-text --remove-whitespace $(EX2)
		@sleep 2 ; echo ; echo "===> once again with hdom tree output" ; echo ; sleep 2
		$(prog) --show-tree --remove-whitespace $(EX2)

test3		:
		@echo "===> namespace processing examples" ; echo ; sleep 2
		@echo "===> namespace propagation test" ; echo ; sleep 2
		$(prog) --verbose --check-namespaces  --indent --output-encoding=UTF-8 $(EX3)
		@echo
		@echo ; sleep 2 ; echo "===> namespace propagation test: tree output with attached namespaces" ; echo ; sleep 2
		$(prog) --verbose --check-namespaces  --remove-whitespace --show-tree --output-encoding=UTF-8 $(EX3)
		@echo
		@echo ; sleep 2 ; echo "===> namespace validation test: this test produces namespace errors" ; echo ; sleep 2
		$(prog) --verbose --do-not-validate --check-namespaces --indent --output-encoding=UTF-8 $(EX3a) || true
		@echo


test4		:
		@echo "===> HTML parsing examples" ; echo ; sleep 2
		@echo "===> the source of a lousy html document" ; echo ; sleep 2
		cat $(EX4)
		@sleep 2 ; echo ; echo "===> expat parser does not parse this document, it will emit an error" ; echo ; sleep 2
		$(prog) --not-lazy --show-tree --trace=1 --parse-html $(EX4)
		@echo
		@sleep 2 ; echo ; echo "===> expat parser does not parse this document, it will not emit an error" ; echo ; sleep 2
		$(prog) --lazy --show-tree --trace=1 --parse-html $(EX4)

clean		:
		rm -f $(prog) *.o *.hi

.PHONY		: all test test0 test1 test2 test3 test4 test5 clean prof local force
