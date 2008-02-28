# $Id: Makefile,v 1.2 2005/04/14 12:52:51 hxml Exp $

HXT_HOME	= ../../..
PKGFLAGS	= 
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

prog		= ./HXPath
prog2		= ./XPathShell

all		: $(prog) $(prog2)

force		:
		$(GHC) --make -o $(prog) $(prog).hs

$(prog)		: $(prog).hs
		$(GHC) --make -o $@ $<

$(prog2)	: $(prog2).hs
		$(GHC) --make -o $@ $<

EX		= ../../xhtml/xhtml.xml

test		: $(prog)
		@echo "===> XPath extraction examples" ; echo ; sleep 2
		@$(MAKE) test1 test2 test3 test4

test1		:
		@echo "===> extract all text from the XHTML document" ; echo ; sleep 2
		$(prog) --do-not-validate "/html/body/descendant-or-self::text()" $(EX)
		@echo

test2		:
		@echo "===> extract the 42. paragraph from the XHTML document" ; echo ; sleep 2
		$(prog) --do-not-validate "/descendant::p[position()=42]" $(EX)
		@echo

test3		:
		@echo "===> extract the text of all top level h1 tags from the XHTML document" ; echo ; sleep 2
		$(prog) --indent --do-not-validate "/child::html/child::body/child::h1/descendant-or-self::text()" $(EX)
		@echo

test4		:
		@echo "===> extract all href attributes from a tags from the XHTML document" ; echo ; sleep 2
		$(prog) --indent --do-not-validate "/descendant::a/attribute::href" $(EX)
		@echo

DIST		= $(HXT_HOME)/dist/examples/arrows
DIST_DIR	= $(DIST)/hxpath

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp Makefile $(prog).hs $(prog2).hs $(DIST_DIR)

clean		:
		rm -f $(prog) *.hi *.o
