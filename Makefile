# $Id: Makefile,v 1.93 2006/11/24 07:41:37 hxml Exp $

include Version.mk

HXT_HOME	= .

DIST		= $(DIST_NAME)
DIST_TAR	= $(DIST_NAME).tar.gz
DIST_FILES	= Makefile Version.mk README LICENCE Setup.lhs $(SOFTWARE).cabal

SETUP		= Setup.lhs

VERSIONTAG	= $(DIST_NAME)
PUBDATE		:= $(shell date +%Y-%m-%d)

EDIT_VERSION	= sed 's/%DISTFILE%/$(DIST_NAME)/g' \
		| sed 's/%VERSION%/$(VERSION)/g' \
		| sed 's/%PUBDATE%/$(PUBDATE)/g'

HSCOLOUR	= HsColour
HSCOLOUR_CSS	= doc/hscolour.css

# --------------------------------------------------

all		:
		$(MAKE) -C src all
		$(MAKE)        $(SOFTWARE).cabal
		# $(MAKE)        allexamples

allexamples	:
		$(MAKE) -C examples all

test		:
		$(MAKE) -C examples test

# ------------------------------------------------------------

CABAL_OPTIONS	= --global

cabal		:
		$(MAKE) cabal_configure cabal_build cabal_doc cabal_install

cabal_configure :
		runhaskell $(SETUP) configure $(CABAL_OPTIONS)

cabal_doc	:
		$(HSCOLOUR) -print-css > $(HSCOLOUR)
		runhaskell $(SETUP) haddock --hyperlink-source --hscolour-css=$(HSCOLOUR)
		rm -f $(HSCOLOUR)

cabal_build	:
		runhaskell $(SETUP) build

cabal_install	:
		sudo runhaskell $(SETUP) install --global

# ------------------------------------------------------------

$(SOFTWARE).cabal	: src/$(SOFTWARE)-package.hs src/Makefile Makefile
		$(MAKE) -C src ../$(SOFTWARE).cabal

DOC_HXT		= $(DIST)/doc/hdoc

# doc brakes because of error in haddock

doc		: $(SOFTWARE).cabal
		$(MAKE) cabal_configure cabal_doc
		[ -d $(DOC_HXT) ] || mkdir -p $(DOC_HXT)
		( cd dist/doc/html/$(SOFTWARE) ; tar cf - . ) | ( cd $(DOC_HXT) ; tar xf - )

distall		: all doc
		$(MAKE) dist

dist		:
		$(MAKE) hxt.cabal
		[ -d $(DIST) ] || mkdir -p $(DIST)
		$(MAKE) -C src      dist
		$(MAKE) -C examples dist
		$(MAKE) -C doc      dist
		[ -d $(DIST)/doc ] || mkdir -p $(DIST)/doc
		cat doc/index.html | $(EDIT_VERSION) > $(DIST)/doc/index.html
		cp $(DIST_FILES) $(DIST)

tarball		: dist
		tar -zcvf $(DIST_TAR) $(DIST)

# ------------------------------------------------------------

clean		:
		$(MAKE) -C src      clean
		$(MAKE) -C examples clean
		$(MAKE) -C doc      clean
		rm -rf $(DIST) $(DIST_TAR) $(SOFTWARE).cabal setup .setup-config .installed-pkg-config dist

# ------------------------------------------------------------

tag		:
		darcs tag $(VERSIONTAG) .

install		:
		$(MAKE) -C src install

uninstall		:
		$(MAKE) -C src uninstall

DISTDATE	= $(shell date -r $(DIST_TAR) +%Y-%m-%d.%R)

distcopy	: $(DIST_TAR)
		scp $(DIST_TAR) hxt@darcs.fh-wedel.de:/home/hxt/$(SOFTWARE)/$(SOFTWARE)-head-$(DISTDATE).tar.gz

# --------------------------------------------------

.PHONY		: all doc test dist clean tag install uninstall tarball webpage cleanwebpage

# --------------------------------------------------

# Create webpage directory for distribution

WEBHOME		= ../../../fh/public_html/HXmlToolbox

webpage		: tarball
		[ ! -d $(WEBHOME) ] || echo "please clean $(WEBHOME) first: make cleanwebpage"
		[ -d $(WEBHOME) ] || mkdir -p $(WEBHOME)
		[ -d $(WEBHOME)/hdoc ] || mkdir -p $(WEBHOME)/hdoc
		cp -r dist/doc/html/$(SOFTWARE)/* $(WEBHOME)/hdoc
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
		cp -f examples/arrows/AGentleIntroductionToHXT/SimpleExamples.hs $(WEBHOME)/examples
		cat doc/index.html | $(EDIT_VERSION) > $(WEBHOME)/index.html
		cp $(DIST_TAR) $(WEBHOME)
		( echo "AddType text/html .html" \
		; echo "allow from all" \
		) > $(WEBHOME)/.htaccess


cleanwebpage	:
		rm -rf $(WEBHOME)


# --------------------------------------------------

# make a clean installation from distribution tar

PROD_DIR	= ~/tmp
INSTALL		= echo now run: "(" cd $(PROD_DIR)/$(DIST) ";" 
INSTEND		= ")"

distbuild	: tarball
		( [ -f $(DIST_TAR) ] || exit 1 \
		; cp -f $(DIST_TAR) $(PROD_DIR) || exit 1 \
		; cd $(PROD_DIR) || exit 1 \
		; rm -rf $(DIST) \
		; tar xvzf $(DIST_TAR) \
		; cd $(DIST) || exit 1 \
		; runhaskell $(SETUP) configure $(CABAL_OPTIONS) || exit 1 \
		; runhaskell $(SETUP) build || exit 1 \
		; $(MAKE) cabal_doc || exit 1 \
		; $(INSTALL) sudo runhaskell $(SETUP) install $(CABAL_OPTIONS) $(INSTEND) \
		)

distinstall	:
		$(MAKE) distbuild INSTALL="" INSTEND=""

# --------------------------------------------------
#

# end Makefile
