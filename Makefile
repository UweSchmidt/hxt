# $Id: Makefile,v 1.93 2006/11/24 07:41:37 hxml Exp $

HXT_HOME	= .

software	= hxt
dist		= $(software)-$(VERSION)
DIST_FILES	= Makefile README LICENCE Setup.lhs hxt.cabal
DIST_TAR	= $(dist).tar.gz

SOFTWARE	= hxt
VERSION		= 7.3
VERSIONTAG	= $(software)-7-03				# use: make tag for creating darcs tag
LASTVERSION	= 7.2
DIST		= $(SOFTWARE)-$(VERSION)
PUBDATE		:= $(shell date +%Y-%m-%d)

EDIT_VERSION	= sed 's/%DISTFILE%/$(DIST)/g' \
		| sed 's/%VERSION%/$(VERSION)/g' \
		| sed 's/%PUBDATE%/$(PUBDATE)/g'


# --------------------------------------------------

all		:
		$(MAKE) -C src      all VERSION=$(VERSION)
		$(MAKE) allexamples

allexamples	:
		$(MAKE) -C examples all VERSION=$(VERSION)

doc		:
		$(MAKE) -C src doc        VERSION=$(VERSION)
		$(MAKE) -C src doc_filter VERSION=$(VERSION)
		$(MAKE) -C src doc_arrow  VERSION=$(VERSION)

test		:
		$(MAKE) -C examples test VERSION=$(VERSION) LASTVERSION=$(LASTVERSION)

setup		: Setup.lhs
		ghc -package Cabal Setup.lhs -o setup
		rm -f Setup.hi Setup.o

hxt.cabal	: src/hxt-package.conf src/Makefile Makefile
		$(MAKE) -C src ../hxt.cabal VERSION=$(VERSION)

dist		: all doc hxt.cabal
		[ -d $(DIST) ] || mkdir -p $(DIST)
		$(MAKE) -C src      dist DIST=../$(DIST)
		$(MAKE) -C examples dist DIST=../$(DIST)
		$(MAKE) -C doc      dist DIST=../$(DIST)
		cat doc/index.html | $(EDIT_VERSION) > $(DIST)/doc/index.html
		cp $(DIST_FILES) $(DIST)

tarball		: dist
		tar -zcvf $(DIST_TAR) $(DIST)

clean		:
		$(MAKE) -C src      clean
		$(MAKE) -C examples clean
		$(MAKE) -C doc      clean
		rm -rf $(DIST) $(DIST_TAR) hxt.cabal setup .setup-config .installed-pkg-config dist

tag		:
		darcs tag $(VERSIONTAG) .

install		:
		$(MAKE) -C src install

uninstall		:
		$(MAKE) -C src uninstall

# --------------------------------------------------

.PHONY		: all doc test dist clean tag install uninstall tarball webpage cleanwebpage

# --------------------------------------------------

# Create webpage directory for distribution

WEBHOME		= ../../../fh/public_html/HXmlToolbox

webpage		: tarball
		[ ! -d $(WEBHOME) ] || echo "please clean $(WEBHOME) first: make cleanwebpage"
		mkdir -p $(WEBHOME)
		cp -r doc/hdoc doc/hdoc_filter doc/hdoc_arrow $(WEBHOME)/
		cp -r doc/hvalidator/thesis $(WEBHOME)/thesis
		cp -f doc/hvalidator/thesis.ps doc/hvalidator/thesis.pdf $(WEBHOME) || true
		[ -d $(WEBHOME)/hxpath ] || mkdir -p $(WEBHOME)/hxpath
		cp -f doc/hxpath/diplomarbeit.pdf $(WEBHOME)/hxpath
		[ -d $(WEBHOME)/relaxng ] || mkdir -p $(WEBHOME)/relaxng
		cp -f doc/relaxng/thesis.pdf $(WEBHOME)/relaxng
		[ -d $(WEBHOME)/cookbook ] || mkdir -p $(WEBHOME)/cookbook
		[ -d $(WEBHOME)/cookbook/doc ] || mkdir -p $(WEBHOME)/cookbook/doc
		cp -f doc/cookbook/doc/thesis.pdf $(WEBHOME)/cookbook/doc
		[ -d $(WEBHOME)/xslt ] || mkdir -p $(WEBHOME)/xslt
		cp -f doc/xslt/thesis.pdf $(WEBHOME)/xslt
		[ -d $(WEBHOME)/examples ] || mkdir -p $(WEBHOME)/examples
		cp -f examples/hsvg/*.html.jpg $(WEBHOME)/examples
		cp -f examples/hsvg/*.html.svg $(WEBHOME)/examples
		cp -f examples/hfilter/FilterExample.hs examples/hunit/HUnitExample.hs examples/hsvg/TreeVisualisation.hs $(WEBHOME)/examples
		cp -f examples/arrows/AGentleIntroductionToHXT/SimpleExamples.hs $(WEBHOME)/examples
		cat doc/index.html | $(EDIT_VERSION) > $(WEBHOME)/index.html
		cp $(DIST_TAR) $(WEBHOME)
		( echo "AddType text/html .html" \
		; echo "allow from all" \
		) > $(WEBHOME)/.htaccess


cleanwebpage	:
		rm -rf $(WEBHOME)


# --------------------------------------------------
#

# end Makefile
