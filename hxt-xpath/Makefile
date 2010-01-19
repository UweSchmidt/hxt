
all		:
		$(MAKE) cabal

allexamples	:
		$(MAKE) -C examples all

test		:
		$(MAKE) -C examples test

# ------------------------------------------------------------

CABAL_OPTIONS	=

cabal		:
		$(MAKE) configure build install doc dist

configure :
		cabal configure $(CABAL_OPTIONS)

doc	:
		cabal haddock  $(CABAL_OPTIONS)

build	:
		cabal build $(CABAL_OPTIONS)

install	:
		cabal install $(CABAL_OPTIONS)

dist	:
		cabal sdist $(CABAL_OPTIONS)

# ------------------------------------------------------------

clean		:
		cabal clean
		$(MAKE) -C examples clean

# ------------------------------------------------------------
# end Makefile
