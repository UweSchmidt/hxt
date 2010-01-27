HXTPACKAGES	= hxt hxt-xpath hxt-xslt hxt-filter hxt-binary hxt-cache

all	:
	$(foreach i,$(HXTPACKAGES), ( cd $i && cabal configure && cabal build && cabal install && cabal sdist; ); )
	ghc-pkg list

global	:
	$(foreach i,$(HXTPACKAGES), ( cd $i && cabal configure && cabal build && cabal sdist && sudo cabal install --global; ); )
	ghc-pkg list

clean	:
	$(foreach i,$(HXTPACKAGES), ( cd $i && cabal clean; ); )

test	:
	[ -d ~/tmp ] || mkdir ~/tmp
	cp test-Makefile ~/tmp/Makefile
	$(foreach i, $(HXTPACKAGES), rm -f $(wildcard ~/tmp/$i-*.tar.gz); )
	$(foreach i, $(HXTPACKAGES), cp $(wildcard $i/dist/$i-*.tar.gz) ~/tmp; )
	$(MAKE) -C ~/tmp all

.PHONY	: all global clean test

