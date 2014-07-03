PL1	= hxt-charproperties \
          hxt-regex-xmlschema \
          hxt-unicode \
          hxt \
          hxt-curl \
          hxt-http \
          hxt-tagsoup \
	  hxt-expat \
          hxt-xpath \
          hxt-relaxng \
          hxt-xmlschema \
	  hxt-xslt \
	  hxt-cache

PL	= $(PL1)

#          janus/janus-library                # no longer maintained
#                 hxt-filter                  # not maintained to work with hxt-9
#                 hxt-binary                  # no longer required, integrated into hxt-9

all	:
	$(foreach i,$(PL), ( cd $i && cabal configure && cabal build && cabal install && cabal sdist; ); )
	@ echo not done: ghc-pkg list

reinstall:
	$(foreach i,$(PL), ( cd $i && cabal install; ); )
	ghc-pkg list

profile:
	$(foreach i,$(PL), ( cd $i && cabal install -p; ); )
	ghc-pkg list

sdist	:
	$(foreach i,$(PL), ( cd $i && cabal sdist; ); )

global	:
	$(foreach i,$(PL), ( cd $i && cabal configure && cabal build && cabal sdist && sudo cabal install --global; ); )
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
	ghc-pkg list --simple-output | xargs --max-args=1 echo | egrep '(hxt(-[a-z]+)?-|janus-library-)' | xargs --max-args=1 ghc-pkg --force unregister
	ghc-pkg list

sb-init	:
	cabal sandbox init --sandbox .cabal-sandbox
	$(foreach i, $(PL), (cd $i && cabal sandbox init --sandbox ../.cabal-sandbox; ); )
	$(foreach i, $(PL), $(foreach j, $(PL), (cd $i && cabal sandbox add-source ../$j; ); ))
	@echo now exec $(MAKE) sb-deps

sb-deps	:
	$(foreach i,$(PL), (cd $i && cabal install --only-dependencies --force-reinstall; ); )

sb-unregister	:
	cabal sandbox hc-pkg list -- --simple-output | xargs --max-args=1 echo | egrep '(hxt(-[a-z]+)?-|janus-library-)' | xargs --max-args=1 cabal sandbox hc-pkg unregister -- --force
	cabal sandbox hc-pkg list


.PHONY	: all reinstall profile sdist global haddock clean test unregister gl-unregister sb-unregister sb-init

