include ../Version.mk

CF		= Janus-Library.cabal

all		:
		runhaskell Setup.hs configure
		runhaskell Setup.hs build
		sudo runhaskell Setup.hs install

# run this target, instead of "all"
# when using hs-plugins for dynamic loading of shaders and handlers

plugins		:
		runhaskell Setup.hs configure -fwithHsPlugins
		runhaskell Setup.hs build
		sudo runhaskell Setup.hs install

clean		:
		runhaskell Setup.hs clean

version		:
		cp $(CF) $(CF)~
		cat $(CF)~ | sed 's|^version:.*|version:	$(VERSION)|' > $(CF)
		( echo "module Network.Server.Janus.ServerVersion" \
		; echo "where" \
		; echo "build_version :: String" \
		; echo "build_version = \"$(VERSION)\"" \
		; echo "build_date    :: String" \
		; echo "build_date    = \"$(DATE)\"" \
		) > source/Network/Server/Janus/ServerVersion.hs
