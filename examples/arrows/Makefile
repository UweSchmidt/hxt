# $Id: Makefile,v 1.9 2006/11/12 14:52:59 hxml Exp $
#
# Example Applications
#

EXAMPLES	= \
		HelloWorld hparser hunit hxpath dtd2hxt absurls \
		pickle \
		hrelaxng \
		RelaxTestSuite \
		AGentleIntroductionToHXT

HXT_HOME	= ../..
PKGFLAGS	=
DIST		= $(HXT_HOME)/dist/examples
DIST_DIR	= $(DIST)/arrows

all		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i PKGFLAGS="$(PKGFLAGS)" $@ ;)

test		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i dist DIST=../$(DIST_DIR) ;)
		cp Makefile $(DIST_DIR)

clean		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $$i $@ ;)

.PHONY		: all clean test dist

