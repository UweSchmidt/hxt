
include Version.mk

# ------------------------------------------------------------

PLUGINDIR	= janus-plugins
LIBDIR		= janus-library
SRVDIR		= janus-server
TESTDIR		= test
VERSIONTAG	= $(SOFTWARE)-$(VERSION)

all		:
		$(MAKE) -C $(LIBDIR)    all
		$(MAKE) -C $(SRVDIR)    all
		$(MAKE) -C $(PLUGINDIR) all
		$(MAKE) test-install

test-install	:
		[ -d "$(TESTDIR)" ] || mkdir   $(TESTDIR)
		cp -vr $(PLUGINDIR)/build/*    $(TESTDIR)
		cp -vr wwwpages                $(TESTDIR)
		cp -vr errorpages              $(TESTDIR)
		cp -vr $(SRVDIR)/examples/conf $(TESTDIR)

run		:
		cd $(TESTDIR) ; janus

clean		:
		cd $(LIBDIR) ; runhaskell Setup.hs clean
		cd $(SRVDIR) ; runhaskell Setup.hs clean
		make -C $(PLUGINDIR) clean
		rm -rf $(TESTDIR)

dist		:
		$(MAKE) version
		$(MAKE) all
version		:
		$(MAKE) -C $(LIBDIR)    version
		$(MAKE) -C $(SRVDIR)    version
		$(MAKE) -C $(PLUGINDIR) version

tag		:
		darcs tag $(VERSIONTAG) .

echo		:
		echo $(DATE)

# ------------------------------------------------------------
