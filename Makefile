HXTPACKAGES	= hxt hxt-filter hxt-binary hxt-cache

all	:
	$(foreach i,$(HXTPACKAGES), cd $i; cabal configure; cabal build; cabal install; )

global	:
	$(foreach i,$(HXTPACKAGES), cd $i; cabal configure; cabal build; sudo cabal install --global; )

clean	:
	$(foreach i,$(HXTPACKAGES), cd $i; cabal clean; )
