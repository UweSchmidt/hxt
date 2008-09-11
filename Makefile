
include Version.mk

# ------------------------------------------------------------

LIBDIR		= janus-library
TESTDIR		= static-janus
DIST            = $(DIST_NAME)
DIST_TAR        = $(DIST_NAME).tar.gz

SOURCES		= README Makefile Version.mk \
		  $(LIBDIR)/LICENSE \
		  $(LIBDIR)/Makefile \
		  $(LIBDIR)/Setup.hs \
		  $(LIBDIR)/Janus-Library.cabal \
		  $(LIBDIR)/source \
		  $(TESTDIR)/Makefile \
		  $(TESTDIR)/conf \
		  $(TESTDIR)/errorpages \
		  $(TESTDIR)/wwwpages \
		  $(TESTDIR)/source \
		  thesis

all		:
		$(MAKE) -C $(LIBDIR)    all
		$(MAKE) -C $(TESTDIR)   all

run		:
		cd $(TESTDIR) ; ./janus

clean		:
		$(MAKE) -C $(LIBDIR) vclean
		$(MAKE) -C $(TESTDIR) clean
		rm -rf dist

dist		: all
		$(MAKE) -C $(TESTDIR) clean
		[ -d $(DIST) ] || mkdir $(DIST)
		tar cvf - $(SOURCES) | ( cd $(DIST) ; tar xvf - )
		tar cvzf $(DIST_TAR) $(DIST)

version		:
		$(MAKE) -C $(LIBDIR)    version

tag		:
		darcs tag $(DIST_NAME) .

PROD_DIR        = ~/tmp
INSTALL         = echo now run: "(" cd $(PROD_DIR)/$(DIST) ";"
INSTEND         = ")"

distbuild	: $(DIST_TAR)
		( [ -f $(DIST_TAR) ] || exit 1 \
                ; cp -f $(DIST_TAR) $(PROD_DIR) || exit 1 \
                ; cd $(PROD_DIR) || exit 1 \
                ; rm -rf $(DIST) \
                ; tar xvzf $(DIST_TAR) \
                ; cd $(DIST) || exit 1 \
                ; $(MAKE) all || exit 1 \
                ; $(MAKE) run \
                )
# ------------------------------------------------------------
