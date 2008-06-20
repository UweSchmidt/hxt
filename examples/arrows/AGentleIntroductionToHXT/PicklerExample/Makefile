# $Id: Makefile,v 1.2 2006/11/17 17:16:24 hxml Exp $
#
# hello world application of Haskell XML Toolbox

HXT_HOME	= ../../../..
PKGFLAGS	= 
GHCFLAGS	= -W -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

DIST		= $(HXT_HOME)/dist/examples/arrows/AGentleIntroductionToHXT
DIST_DIR	= $(DIST)/PicklerExample

src		= Baseball.hs
prog		= ./Baseball

all		:
		$(MAKE) $(prog)

force		:
		$(MAKE) distclean all

test		: $(prog)
		$(prog)

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp $(src) simple2.xml new-simple2.xml Makefile $(DIST_DIR)

clean		:
		rm -f *.o *.hi

distclean	:
		$(MAKE) clean
		rm -f $(prog)

.PHONY		: all test dist clean distclean force

$(prog)		: $(src)
		$(GHC) --make -o $@ $<


