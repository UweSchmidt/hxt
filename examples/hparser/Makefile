# $Id: Makefile,v 1.7 2005/04/14 12:52:51 hxml Exp $

HXT_HOME	= ../..
PKGFLAGS	=
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

prog	= HXmlParser

all	: $(prog)

HXmlParser	: HXmlParser.hs
		$(GHC) --make -o $@ $<

force		:
		$(GHC) --make -o $(prog) $(prog).hs

test		:
		@echo "===> run a few simple test cases with the validating parser"
		@echo "===> first see all command line options"
		./HXmlParser --help
		@echo
		$(MAKE) test0 test1 test2 test3 test4

EX1		= ./example1.xml
EXi		= ./invalid.xml
EX2		= ../xhtml/xhtml.xml
EX3		= ./namespace0.xml
EX3a		= ./namespace1.xml
EX4		= ./lousy.html

EX		= $(EX1) $(EXi) $(EX3) $(EX3a) $(EX4)

test0		: HXmlParser
		@echo "===> a 1. simple valid document"
		./HXmlParser $(EX1)
		@echo
		@echo "===> the dom tree of the same document (without any redundant whitespace)"
		./HXmlParser --show-tree --remove-whitespace $(EX1)
		@echo
		@echo "===> the next test case contains validation erors, it must fail"
		./HXmlParser --verbose $(EXi) || true
		@echo
		@echo "===> same source, but only wellformed check"
		./HXmlParser --do-not-validate $(EXi) || true
		@echo
		@echo "===> only validation, no output of an XHTML source"
		./HXmlParser --verbose --no-output $(EX2)
		@echo

test1		: HXmlParser
		@echo "===> the source of a very simple valid document" ; echo ; sleep 2
		cat $(EX1)
		@sleep 2 ; echo ; echo "===> parser will emit UTF-8" ; echo ; sleep 2
		./HXmlParser --output-encoding=UTF-8 $(EX1)
		@echo
		@sleep 2 ; echo ; echo "===> once again with ISO-8859-1 (latin1) output" ; echo ; sleep 2
		./HXmlParser --output-encoding=ISO-8859-1 $(EX1)
		@echo
		@sleep 2 ; echo ; echo "===> once again with US-ASCII output" ; echo ; sleep 2
		./HXmlParser --output-encoding=US-ASCII $(EX1)
		@echo
		@sleep 2 ; echo ; echo "===> once again with hdom tree output" ; echo ; sleep 2
		./HXmlParser --show-tree --output-encoding=ISO-8859-1 $(EX1)
		@echo
		@sleep 2 ; echo ; echo "===> once again, but without any markup" ; echo ; sleep 2
		./HXmlParser --show-text --output-encoding=ISO-8859-1 $(EX1)
		@echo

test2		: HXmlParser
		@echo "===> the source of a xhtml document" ; echo ; sleep 2
		cat $(EX2)
		@echo "that document has" `cat $(EX2) | wc -l` "lines"
		@sleep 2 ; echo ; echo "===> parser will validate this document and try to indent the output" ; echo ; sleep 2
		./HXmlParser --indent $(EX2)
		@sleep 2 ; echo ; echo "===> once again, but remove all markup" ; echo ; sleep 2
		./HXmlParser --show-text --remove-whitespace $(EX2)
		@sleep 2 ; echo ; echo "===> once again with hdom tree output" ; echo ; sleep 2
		./HXmlParser --show-tree --remove-whitespace $(EX2)

test3		: HXmlParser
		@echo "===> namespace processing examples" ; echo ; sleep 2
		@echo "===> namespace propagation test" ; echo ; sleep 2
		./HXmlParser --verbose --check-namespaces  --indent --output-encoding=ISO-8859-1 $(EX3)
		@echo
		@echo ; sleep 2 ; echo "===> namespace propagation test: tree output with attached namespaces" ; echo ; sleep 2
		./HXmlParser --verbose --check-namespaces  --remove-whitespace --show-tree --output-encoding=ISO-8859-1 $(EX3)
		@echo
		@echo ; sleep 2 ; echo "===> namespace validation test: this test produces namespace errors" ; echo ; sleep 2
		./HXmlParser --verbose --do-not-validate --check-namespaces --indent --output-encoding=ISO-8859-1 $(EX3a) || true
		@echo


test4		: HXmlParser
		@echo "===> HTML parsing examples" ; echo ; sleep 2
		@echo "===> the source of a lousy html document" ; echo ; sleep 2
		cat $(EX4)
		@sleep 2 ; echo ; echo "===> parser accepts this document and tries to build a document tree" ; echo ; sleep 2
		./HXmlParser --indent --preserve-comment --parse-html $(EX4)
		@echo

DIST		= $(HXT_HOME)/dist/examples
DIST_DIR	= $(DIST)/hparser

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp $(EX) Makefile $(prog).hs $(DIST_DIR)

clean		:
		rm -f $(prog) *.o *.hi

