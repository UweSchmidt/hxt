SOFTWARE	= janus
VERSION		= 1.1.1
VERSIONTAG	= $(SOFTWARE)-$(VERSION)

# ------------------------------------------------------------

PLUGINDIR	= janus-plugins
LIBDIR		= janus-library
SRVDIR		= janus-server
TESTDIR		= test

all		:
		$(MAKE) janus-lib
		$(MAKE) janus-srv
		$(MAKE) janus-plug
		$(MAKE) test-install

janus-lib	:
		( cd $(LIBDIR) \
		; runhaskell Setup.hs configure \
		; runhaskell Setup.hs build \
		; sudo runhaskell Setup.hs install \
		)

janus-srv	:
		( cd $(SRVDIR) \
		; runhaskell Setup.hs configure \
		; runhaskell Setup.hs build \
		; sudo runhaskell Setup.hs install \
		)

janus-plug	:
		make -C $(PLUGINDIR) all

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

tag		:
		darcs tag $(VERSIONTAG) .

# ------------------------------------------------------------
