# $Id: Makefile,v 1.15 2006/11/12 14:52:59 hxml Exp $
#
# Example Applications
#

include ../Version.mk

HXT_HOME	= ..

EXAMPLES	= arrows
MORE_SAMPLES	= 
DIST		= ../$(DIST_NAME)
DIST_DIR	= $(DIST)/examples

all		:
		if [ -f $(HXT_HOME)/lib/hxt/package.conf ] ; \
		then $(MAKE) alllocal ; \
		else $(MAKE) alldist ; \
		fi 

alllocal		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i PKGFLAGS="-package-conf `pwd`/$(HXT_HOME)/lib/hxt/package.conf -package hxt-$(VERSION)" all ;)
alldist		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i all ;)

test		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)

clean		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)

.PHONY		: all alllocal alldist test dist clean

