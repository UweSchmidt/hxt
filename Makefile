PL1	= hxt-charproperties \
          hxt-regex-xmlschema \
          hxt-unicode \
          hxt \
          hxt-curl \
          hxt-http \
          hxt-tagsoup \
          hxt-xpath \
          hxt-relaxng \
          hxt-xmlschema \
	  hxt-xslt \
	  hxt-cache

PL	= $(PL1)

# hxt-expat-0.20.7 is not yet ready for ghc-7.10 because of dependency of deepseq < 1.4
PL2     = $(PL1) hxt-expat


FLAGS=--flags="network-uri"

#          janus/janus-library                # no longer maintained
#                 hxt-filter                  # not maintained to work with hxt-9
#                 hxt-binary                  # no longer required, integrated into hxt-9

all	:
	$(foreach i,$(PL), ( cd $i && cabal configure $(FLAGS)\
                                   && cabal build\
                                   && cabal install $(FLAGS)\
                                   && cabal sdist; ); )
	@ echo not done: ghc-pkg list

install:
	$(foreach i,$(PL), ( cd $i && cabal install $(FLAGS) ; ); )
	$(MAKE) list

profile:
	$(foreach i,$(PL), ( cd $i && cabal install $FLAGS) -p; ); )
	$(MAKE) list

sdist	:
	$(foreach i,$(PL), ( cd $i && cabal sdist; ); )

list	:
	( [ -d .cabal-sandbox ] && cabal sandbox hc-pkg list ) || ghc-pkg list

global	:
	$(foreach i,$(PL), ( cd $i && cabal configure  $(FLAGS) && cabal build && cabal sdist && sudo cabal install  $(FLAGS) --global; ); )
	ghc-pkg list

haddock	:
	$(foreach i,$(PL), ( cd $i && cabal haddock ); )

clean	:
	$(foreach i,$(PL), ( cd $i && cabal clean; ); )

test	:
	[ -d ~/tmp ] || mkdir ~/tmp
	cp test-Makefile ~/tmp/Makefile
	$(foreach i, $(PL1), rm -f $(wildcard ~/tmp/$i-*.tar.gz); )
	$(foreach i, $(PL1), cp $(wildcard $i/dist/$i-*.tar.gz) ~/tmp; )
	$(MAKE) -C ~/tmp all

unregister	:
	if [ -d .cabal-sandbox ]; \
	then $(MAKE) sb-unregister; \
	else $(MAKE) gl-unregister; \
	fi

gl-unregister	:
	ghc-pkg list --simple-output \
		| xargs --max-args=1 echo \
		| egrep '(hxt(-[a-z]+)?-|janus-library-)' \
		| xargs --max-args=1 ghc-pkg --force unregister
	ghc-pkg list

sb-init	:
	cabal sandbox init --sandbox .cabal-sandbox
	$(foreach i, $(PL), (cd $i && cabal sandbox init --sandbox ../.cabal-sandbox; ); )
	$(foreach i, $(PL), (         cabal sandbox add-source $i; ); )
	@echo now exec $(MAKE) sb-deps

sb-deps	:
	$(foreach i,$(PL), (cd $i && cabal install --only-dependencies $(FLAGS) ; ); )

sb-unregister	:
	cabal sandbox hc-pkg list -- --simple-output \
		| xargs --max-args=1 echo \
		| egrep '(hxt(-[a-z]+)?-|janus-library-)' \
		| xargs --max-args=1 cabal sandbox hc-pkg unregister -- --force
	cabal sandbox hc-pkg list

sb-delete	:
	cabal sandbox delete
	rm -f */cabal.sandbox.config

.PHONY	: all reinstall profile sdist global haddock clean test \
		unregister gl-unregister sb-unregister sb-init sb-delete

