# $Id: Makefile,v 1.9 2006/11/12 14:52:59 hxml Exp $
#
# Example Applications
#

EXAMPLES	= \
		HelloWorld hparser hunit hxpath dtd2hxt absurls \
		hrelaxng \
		RelaxTestSuite \
		AGentleIntroductionToHXT

HXT_HOME	= ../..
PKGFLAGS	=
DIST		= $(HXT_HOME)/dist/examples
DIST_DIR	= $(DIST)/arrows

all		:
		for i in $(EXAMPLES) ; do $(MAKE) -C $$i PKGFLAGS="$(PKGFLAGS)" $@ ; done

test		:
		for i in $(EXAMPLES) ; do $(MAKE) -C $$i $@ ; done

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		for i in $(EXAMPLES) ; do $(MAKE) -C $$i dist DIST=../$(DIST_DIR) ; done
		cp Makefile $(DIST_DIR)

clean		:
		for i in $(EXAMPLES) ; do $(MAKE) -C $$i $@ ; done

.PHONY		: all clean test dist

