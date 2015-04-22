all	: colorize

install	: all
	sudo cp colorize /usr/local/bin

clean	:
	cabal clean

colorize:
	cabal install

.PHONY	: all install clean colorize
 

