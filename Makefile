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
		$(MAKE)        allexamples

allexamples	:
		$(MAKE) -C examples all

test		:
		$(MAKE) -C examples test

# ------------------------------------------------------------

cabal		:
		$(MAKE) cabal_configure cabal_build cabal_doc cabal_install

cabal_configure :
		runhaskell $(SETUP) configure

cabal_doc	:
		$(HSCOLOUR) -print-css > $(HSCOLOUR)
		runhaskell $(SETUP) haddock --hyperlink-source --hscolour-css=$(HSCOLOUR)
		rm -f $(HSCOLOUR)

cabal_build	:
		runhaskell $(SETUP) build

cabal_install	:
		sudo runhaskell $(SETUP) install

# ------------------------------------------------------------

$(SOFTWARE).cabal	: src/$(SOFTWARE)-package.hs src/Makefile Makefile
		$(MAKE) -C src ../$(SOFTWARE).cabal

DOC_HXT		= $(DIST)/doc/$(SOFTWARE)

doc		: $(SOFTWARE).cabal
		$(MAKE) cabal_configure cabal_doc
		[ -d $(DOC_HXT) ] || mkdir -p $(DOC_HXT)
		( cd dist/doc/html/$(SOFTWARE) ; tar cf - . ) | ( cd $(DOC_HXT) ; tar xf - )

dist		: all doc
		[ -d $(DIST) ] || mkdir -p $(DIST)
		$(MAKE) -C src      dist
		$(MAKE) -C examples dist
		cp $(DIST_FILES) $(DIST)

tarball		: dist
		tar -zcvf $(DIST_TAR) $(DIST)

# ------------------------------------------------------------

clean		:
		$(MAKE) -C src      clean
		$(MAKE) -C examples clean
		rm -rf $(DIST) $(DIST_TAR) $(SOFTWARE).cabal .setup-config .installed-pkg-config dist

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
		[ -d $(WEBHOME) ] || exit 1
		[ -d $(WEBHOME)/hdoc-filter ] || mkdir -p $(WEBHOME)/hdoc-filter
		cp -r dist/doc/html/$(SOFTWARE)/* $(WEBHOME)/hdoc-filter
		cp $(DIST_TAR) $(WEBHOME)

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
		; runhaskell $(SETUP) configure || exit 1 \
		; runhaskell $(SETUP) build || exit 1 \
		; $(INSTALL) sudo runhaskell $(SETUP) install $(INSTEND) \
		)

distinstall	:
		$(MAKE) distbuild INSTALL="" INSTEND=""

# --------------------------------------------------
#

# end Makefile
