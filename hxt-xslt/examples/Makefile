# $Id: Makefile,v 1.15 2006/11/12 14:52:59 hxml Exp $
#
# Example Applications
#

HXT_HOME	= ..

EXAMPLES	= hparser
MORE_SAMPLES	= 
DIST		= ../$(DIST_NAME)
DIST_DIR	= $(DIST)/examples

all		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i all ;)

test		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)

clean		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)

.PHONY		: all test clean

