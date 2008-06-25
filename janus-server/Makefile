include ../Version.mk

CF		= Janus-Server.cabal

all		:
		runhaskell Setup.hs configure
		runhaskell Setup.hs build
		sudo runhaskell Setup.hs install

clean		:
		runhaskell Setup.hs clean

version		:
		cp $(CF) $(CF)~
		cat $(CF)~ | sed 's|^version:.*|version:	$(VERSION)|' > $(CF)
