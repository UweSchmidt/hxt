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
		$(MAKE) -C doc      dist
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

# --------------------------------------------------
#

# end Makefile
