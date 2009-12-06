# $Id: Makefile,v 1.3 2005/04/14 12:52:51 hxml Exp $

HXT_HOME	= ../..
PACKAGE_DIR	= $(HXT_HOME)/lib/hxt
# PKGFLAGS	= -package-conf $(PACKAGE_DIR)/package.conf -package hxt
PKGFLAGS	=
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

ifeq ($(HOST),si)
OPTS		= --trace=1 --use-curl --proxy www-cache.fh-wedel.de:3128
else
OPTS		= --trace=1 --use-curl
endif

prog		= ./Citations

all		: $(prog)

$(prog)		: $(prog).hs
		$(GHC) --make $(GHCPACKAGES) -o $@ $<

force		:
		$(GHC) --make $(GHCPACKAGES) -o $(prog) $(prog).hs

test		:
		@echo "===> get cites from www.blutgraetsche.de"
		$(MAKE) Zitate.hs

Zitate.hs	: $(prog) Makefile
		$(prog) $(OPTS) -f $@

clean		:
		rm -f $(prog)

