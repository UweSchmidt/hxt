# $Id: Makefile,v 1.5 2006/11/11 15:36:03 hxml Exp $

HXT_HOME	= ..

all		:
		@echo "thesis not remade, command '$(MAKE) -C doc/hvalidator all' not executed (this is o.k)"

test		:

DIST		= $(HXT_HOME)/dist
DIST_DIR	= $(DIST)/doc
DIST_FILES	= Makefile \
		  hdoc hdoc_filter hdoc_arrow \
		  hvalidator/thesis \
		  $(wildcard hxpath/*.pdf) \
		  relaxng/thesis.pdf \
		  xslt/thesis.pdf \
		  cookbook/Makefile $(wildcard cookbook/*.hs) $(wildcard cookbook/examples/*.rdf) cookbook/doc/thesis.pdf

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		tar cvf - $(DIST_FILES) | ( cd $(DIST_DIR) ; tar xf - )
		cp hvalidator/thesis.ps	$(DIST_DIR)

clean		:
		rm -rf hdoc/* hdoc_arrow/* hdoc_filter/*

.PHONY		: all test dist clean
