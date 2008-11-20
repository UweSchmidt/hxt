include ../Version.mk

CF		= Janus-Library.cabal
VF		= source/Network/Server/Janus/ServerVersion.hs

all		: version
		runhaskell Setup.hs configure --global
		runhaskell Setup.hs build
		sudo runhaskell Setup.hs install --global

# run this target, instead of "all"
# when using hs-plugins for dynamic loading of shaders and handlers

plugins		: version
		runhaskell Setup.hs configure -fwithHsPlugins
		runhaskell Setup.hs build
		sudo runhaskell Setup.hs install

clean		:
		runhaskell Setup.hs clean

version		: $(VF)

$(VF)		: ../Version.mk
		cp $(CF) $(CF)~
		cat $(CF)~ | sed 's|^version:.*|version:	$(VERSION)|' > $(CF)
		( echo "module Network.Server.Janus.ServerVersion" \
		; echo "where" \
		; echo "" \
		; echo "build_version :: String" \
		; echo "build_version = \"$(VERSION)\"" \
		) > $@

janus-conf	: JanusConf.hs
		ghc -Wall --make -o $@ $<

