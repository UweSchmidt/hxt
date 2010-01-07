HXTPACKAGES	= hxt hxt-xslt hxt-filter hxt-binary hxt-cache

all	:
	$(foreach i,$(HXTPACKAGES), ( cd $i && cabal configure && cabal build && cabal install; ) ; )
	ghc-pkg list

global	:
	$(foreach i,$(HXTPACKAGES), ( cd $i && cabal configure && cabal build && sudo cabal install --global; ) )
	ghc-pkg list

clean	:
	$(foreach i,$(HXTPACKAGES), ( cd $i && cabal clean; ) )
