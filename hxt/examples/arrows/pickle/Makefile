# $Id: Makefile,v 1.3 2005/04/14 12:52:50 hxml Exp $
#
# hello world application of Haskell XML Toolbox

HXT_HOME	= ../../..
PKGFLAGS	= 
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

DIST		= $(HXT_HOME)/dist/examples/arrows
DIST_DIR	= $(DIST)/pickle

PROG		= ./pickleTest ./pickleTestWithNamespaces

all		: $(PROG)

force		:
		$(MAKE) distclean all

test		:
		./pickleTest
		@sleep 1
		@echo "the program p2 as XML document"
		@sleep 3
		cat pickle.xml
		./pickleTestWithNamespaces
		@sleep 1
		@echo "the program p2 as XML document"
		@sleep 3
		cat pickle.xml

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp PickleTest.hs Makefile $(DIST_DIR)

clean		:
		rm -f *.o *.hi pickle.xml $(PROG)

distclean	:
		$(MAKE) clean

.PHONY		: all test dist clean distclean force

pickleTest	: PickleTest.hs
		$(GHC) --make -o $@ $<

pickleTestWithNamespaces	: PickleTestWithNamespaces.hs
		$(GHC) --make -o $@ $<
