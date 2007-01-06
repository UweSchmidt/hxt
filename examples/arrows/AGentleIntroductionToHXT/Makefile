# $Id: Makefile,v 1.2 2006/11/17 17:16:24 hxml Exp $
#
# hello world application of Haskell XML Toolbox

HXT_HOME	= ../../..
PKGFLAGS	= 
GHCFLAGS	= -W -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

DIST		= $(HXT_HOME)/dist/examples/arrows
DIST_DIR	= $(DIST)/AGentleIntroductionToHXT

src		= SimpleExamples.hs
prog		= SimpleExamples

tests		= \
		selectAllText \
		selectAllTextAndAltValues \
		selectAllTextAndRealAltValues \
		addRefIcon \
		helloWorld \
		helloWorld2 \
		imageTable \
		imageTable0 \
		imageTable1 \
		imageTable2 \
		imageTable3 \
		toAbsHRefs \
		toAbsRefs \
		toAbsRefs1

all		: $(prog)

force		:
		$(MAKE) distclean all

test		:
		for op in $(tests) ; \
		do \
			$(prog) $$op "http://www.haskell.org/" "-" ; \
		done

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp $(src) Makefile $(DIST_DIR)

clean		:
		rm -f *.o *.hi

distclean	:
		$(MAKE) clean
		rm -f mini hello

.PHONY		: all test dist clean distclean force

$(prog)		: $(src)
		$(GHC) --make -o $@ $<


