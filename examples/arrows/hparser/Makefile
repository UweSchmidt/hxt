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
		ghc --make -o $(prog) -Wall -O -fglasgow-exts -ignore-package hxt -i../../../src $(prog).hs

$(prog)		: HXmlParser.hs
		$(GHC) --make -o $@ $<

force		:
		$(GHC) --make -o $(prog) $(prog).hs

test		: $(prog)
		@echo "===> run a few simple test cases with the validating parser"
		@echo "===> first see all command line options"
		$(prog) --help
		@echo
		$(MAKE) test0 test1 test2 test3 test4 test5 xslttest

EX1		= ./example1.xml
EXi		= ./invalid.xml
EX2		= ../../xhtml/xhtml.xml
EX3		= ./namespace0.xml
EX3a		= ./namespace1.xml
EX4		= ./lousy.html

EX		= $(wildcard example*.xml) $(wildcard lousy*.html) $(wildcard *valid*.xml *valid*.rng) $(wildcard namespace*.xml)

test0		:
		@echo "===> a 1. simple valid document"
		$(prog) $(EX1)
		@echo
		@echo "===> the dom tree of the same document (without any redundant whitespace)"
		$(prog) --show-tree --remove-whitespace $(EX1)
		@echo
		@echo "===> the next test case contains validation erors, it must fail"
		$(prog) --verbose $(EXi) || true
		@echo
		@echo "===> same source, but only wellformed check"
		$(prog) --do-not-validate $(EXi) || true
		@echo
		@echo "===> only validation, no output of an XHTML source"
		$(prog) --verbose --no-output $(EX2)
		@echo

test1		:
		@echo "===> the source of a very simple valid document" ; echo ; sleep 2
		cat $(EX1)
		@sleep 2 ; echo ; echo "===> parser will emit UTF-8" ; echo ; sleep 2
		$(prog) --output-encoding=UTF-8 $(EX1)
		@echo
		@sleep 2 ; echo ; echo "===> once again with ISO-8859-1 (latin1) output" ; echo ; sleep 2
		$(prog) --output-encoding=ISO-8859-1 $(EX1)
		@echo
		@sleep 2 ; echo ; echo "===> once again with US-ASCII output" ; echo ; sleep 2
		$(prog) --output-encoding=US-ASCII $(EX1)
		@echo
		@sleep 2 ; echo ; echo "===> once again with hdom tree output" ; echo ; sleep 2
		$(prog) --show-tree --output-encoding=ISO-8859-1 $(EX1)
		@echo
		@sleep 2 ; echo ; echo "===> once again, but without any markup" ; echo ; sleep 2
		$(prog) --show-text --output-encoding=ISO-8859-1 $(EX1)
		@echo

test2		:
		@echo "===> the source of a xhtml document" ; echo ; sleep 2
		cat $(EX2)
		@echo "that document has" `cat $(EX2) | wc -l` "lines"
		@sleep 2 ; echo ; echo "===> parser will validate this document and try to indent the output" ; echo ; sleep 2
		$(prog) --indent $(EX2)
		@sleep 2 ; echo ; echo "===> once again, but remove all markup" ; echo ; sleep 2
		$(prog) --show-text --remove-whitespace $(EX2)
		@sleep 2 ; echo ; echo "===> once again with hdom tree output" ; echo ; sleep 2
		$(prog) --show-tree --remove-whitespace $(EX2)

test3		:
		@echo "===> namespace processing examples" ; echo ; sleep 2
		@echo "===> namespace propagation test" ; echo ; sleep 2
		$(prog) --verbose --check-namespaces  --indent --output-encoding=ISO-8859-1 $(EX3)
		@echo
		@echo ; sleep 2 ; echo "===> namespace propagation test: tree output with attached namespaces" ; echo ; sleep 2
		$(prog) --verbose --check-namespaces  --remove-whitespace --show-tree --output-encoding=ISO-8859-1 $(EX3)
		@echo
		@echo ; sleep 2 ; echo "===> namespace validation test: this test produces namespace errors" ; echo ; sleep 2
		$(prog) --verbose --do-not-validate --check-namespaces --indent --output-encoding=ISO-8859-1 $(EX3a) || true
		@echo


test4		:
		@echo "===> HTML parsing examples" ; echo ; sleep 2
		@echo "===> the source of a lousy html document" ; echo ; sleep 2
		cat $(EX4)
		@sleep 2 ; echo ; echo "===> parser accepts this document and tries to build a document tree" ; echo ; sleep 2
		$(prog) --indent --preserve-comment --parse-html $(EX4)
		@echo

test5		:
		@echo "===> Validate a valid xml file with respect to a valid Relax NG schema" ; echo ; sleep 1
		@echo "the XML document ./valid1.xml"
		@cat ./valid1.xml
		@echo "the Relax NG schema ./valid1.rng"
		@cat  ./valid1.rng
		@echo
		./$(prog) --relax-schema ./valid1.rng ./valid1.xml
		@echo
		@echo "===> Validate an  invalid xml file with respect to a valid Relax NG schema" ; echo ; sleep 1
		@echo "the invalid XML document ./invalid1.xml"
		@cat ./valid1.xml
		./$(prog) --relax-schema ./valid1.rng ./invalid1.xml || true
		@echo
		@echo "===> Incorrect Relax NG schema" ; echo ; sleep 1
		./$(prog) --relax-schema ./invalid2.rng ./valid1.xml || true
		@echo
		@echo "===> Incorrect Relax NG schema with two errors" ; echo ; sleep 1
		./$(prog) --relax-schema ./invalid3.rng ./valid1.xml || true
		@echo


xslttest	:
		$(MAKE) -C xslt test

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp $(EX) Makefile $(prog).hs $(DIST_DIR)
		for i in xslt ; do $(MAKE) -C $$i dist DIST=../$(DIST_DIR) ; done


clean		:
		rm -f $(prog) *.o *.hi

