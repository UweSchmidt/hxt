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
          hxt-xmlschema\
	  hxt-xslt \
	  hxt-cache

PL	= $(PL1) \
          janus/janus-library

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
	ghc-pkg list --simple-output | xargs --max-args=1 echo | egrep '(hxt(-[a-z]+)?-|janus-library-)' | xargs --max-args=1 ghc-pkg --force unregister
	ghc-pkg list

.PHONY	: all global clean test

