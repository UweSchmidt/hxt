# $Id: Makefile,v 1.15 2006/11/12 14:52:59 hxml Exp $
#
# Example Applications
#

HXT_HOME	= ..
VERSION		= 7.3
LASTVERSION	= 7.2

EXAMPLES	= mini hparser hxpath hfilter hunit hsvg dtd2haskell arrows
MORE_SAMPLES	= w3ctest blutgraetsche photoalbum
DIST		= $(HXT_HOME)/dist
DIST_DIR	= $(DIST)/examples

all		:
		if [ -f $(HXT_HOME)/lib/hxt/package.conf ] ; \
		then $(MAKE) alllocal ; \
		else $(MAKE) alldist ; \
		fi 

alllocal		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i PKGFLAGS="-ignore-package hxt-$(LASTVERSION) -package-conf `pwd`/$(HXT_HOME)/lib/hxt/package.conf -package hxt" all ;)
alldist		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i all ;)

test		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		for i in $(EXAMPLES) ; do $(MAKE) -C $$i dist DIST=../$(DIST_DIR) ; done
		[ -d $(DIST_DIR)/xhtml ] || mkdir -p $(DIST_DIR)/xhtml
		cp xhtml/xhtml*.dtd xhtml/xhtml*.ent xhtml/xhtml*.xml $(DIST_DIR)/xhtml
		cp Makefile $(DIST_DIR)

clean		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)

RunTestCases	:
		$(MAKE) -C w3ctest $@

# under construction

hxslt/runXslt	: hxslt/runXslt.hs
		$(GHC) --make -o $@ $<

.PHONY		: all clean test dist RunTestCases

