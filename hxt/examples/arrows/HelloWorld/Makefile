# $Id: Makefile,v 1.3 2005/04/14 12:52:50 hxml Exp $
#
# hello world application of Haskell XML Toolbox

HXT_HOME	= ../../..
PKGFLAGS	= 
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

DIST		= $(HXT_HOME)/dist/examples/arrows
DIST_DIR	= $(DIST)/HelloWorld

all		: mini hello

force		:
		$(MAKE) distclean all

test		:
		./mini
		./hello

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp Mini.hs HelloWorld.hs hello.xml Makefile $(DIST_DIR)

clean		:
		rm -f *.o *.hi

distclean	:
		$(MAKE) clean
		rm -f mini hello

.PHONY		: all test dist clean distclean force

hello		: HelloWorld.hs
		$(GHC) --make -o $@ $<

mini		: Mini.hs
		$(GHC) --make -o $@ $<

