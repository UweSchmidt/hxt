# $Id: Makefile,v 1.1 2006/05/24 12:47:25 hxml Exp $

HXT_HOME	= ../..
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS)

ifeq ($(HOST),si)
OPTS		= --trace=1 --use-curl --proxy www-cache.fh-wedel.de:3128
else
OPTS		= --trace=1 --use-curl
endif

prog		= Citations

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

