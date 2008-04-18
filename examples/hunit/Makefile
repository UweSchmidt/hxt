# $Id: Makefile,v 1.6 2005/04/14 12:52:51 hxml Exp $

HXT_HOME	= ../..
PKGFLAGS	=
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

prog		= ./HUnitExample

all		: $(prog)

force		:
		$(GHC) --make $(GHCPACKAGES) -o $(prog) $(prog).hs

$(prog)		: $(prog).hs
		$(GHC) --make $(GHCPACKAGES) -o $@ $<

test		: $(prog)
		@echo "===> run HUnit examples" ; echo ; sleep 2
		$(prog)

DIST		= $(HXT_HOME)/dist/examples
DIST_DIR	= $(DIST)/hunit
DIST_FILES	= mini1.xml mini2.xml Makefile $(prog).hs

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp $(DIST_FILES) $(DIST_DIR)

clean		:
		rm -f $(prog) $(prog).o $(prog).hi

