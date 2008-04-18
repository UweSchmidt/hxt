# Example Applications
#

include ../Version.mk

HXT_HOME	= ..

EXAMPLES	= mini hparser hfilter hunit hsvg dtd2haskell
MORE_SAMPLES	= blutgraetsche photoalbum
DIST		= ../$(DIST_NAME)
DIST_DIR	= $(DIST)/examples

all		:
		if [ -f $(HXT_HOME)/lib/$(SOFTWARE)/package.conf ] ; \
		then $(MAKE) alllocal ; \
		else $(MAKE) alldist ; \
		fi 

alllocal		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i PKGFLAGS="-ignore-package $(SOFTWARE)-$(LASTVERSION) -package-conf `pwd`/$(HXT_HOME)/lib/$(SOFTWARE)/package.conf -package $(SOFTWARE)" all ;)

alldist		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i all ;)

test		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i dist DIST=../$(DIST_DIR);)
		[ -d $(DIST_DIR)/xhtml ] || mkdir -p $(DIST_DIR)/xhtml
		cp xhtml/xhtml*.dtd xhtml/xhtml*.ent xhtml/xhtml*.xml $(DIST_DIR)/xhtml
		cp Makefile $(DIST_DIR)

clean		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)


.PHONY		: all clean test dist

