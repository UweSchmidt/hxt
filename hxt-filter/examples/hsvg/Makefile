# $Id: Makefile,v 1.7 2006/06/01 15:03:21 hxml Exp $

HXT_HOME	= ../..
PKGFLAGS	=
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

prog		= ./TreeVisualisation

all		: $(prog)

force		:
		$(GHC) --make -o $(prog) $(prog).hs

$(prog)		: $(prog).hs
		$(GHC) --make -o $@ $<

# Tree Visualisation Examples

proxy		:= $(shell if [ `hostname` = "si" ] ; then echo -n "--proxy=www-cache.fh-wedel.de:3128" ; else echo -n ; fi)

HTV		= $(prog) -t1 --use-curl --options-curl="-L" --encoding=ISO-8859-1 --parse-html --do-not-issue-warnings --remove-whitespace
XTV		= $(prog) -t1 --use-curl --do-not-validate --do-not-issue-warnings --remove-whitespace

JPGS		= index.html.jpg \
		  www.fh-wedel.de.index.html.jpg \
		  www.w3.org.index.html.jpg \
		  www.haskell.org.index.html.jpg

vtree		: $(prog) $(JPGS)

index.html.svg	: $(HXT_HOME)/doc/index.html
		@echo "get Haskell XML Toolbox homepage"
		$(HTV) --width=800 --height=400 \
			 $< > $@

www.fh-wedel.de.index.html.svg	:
		@echo "get FH Wedel homepage"
		$(HTV) --horizontalTree --horizontalLines --width=600 --height=800 --border 30 \
			 http://www.fh-wedel.de/ > $@

www.w3.org.index.html.svg	:
		@echo "get www.w3.org"
		$(XTV) --centricTree --withAttr --width=1000 --height=1000 $(proxy) \
			 http://www.w3.org/ > $@

www.haskell.org.index.html.svg	:
		@echo "get www.haskell.org"
		$(HTV) --horizontalLines --withAttr --width=600 --height=400 $(proxy) \
			 http://www.haskell.org/haskellwiki/Haskell > $@

%.jpg		: %.svg
		svgtoraster -m image/jpeg -q 0.7 `pwd`/$<

%.png		: %.svg
		svgtoraster -m image/png `pwd`/$<


test		:
		$(MAKE) index.html.svg

DIST		= $(HXT_HOME)/dist/examples
DIST_DIR	= $(DIST)/hsvg
DIST_FILES	= $(prog).hs Makefile .ghci

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp $(DIST_FILES) $(DIST_DIR)

clean		:
		rm -f $(prog) *.o *.hi *.jpg *.png

cleanvt		:
		rm -f \
		  index.html.jpg \
		  www.fh-wedel.de.index.html.jpg \
		  www.w3.org.index.html.jpg \
		  www.haskell.org.index.html.jpg \
		  index.html.svg \
		  www.fh-wedel.de.index.html.svg \
		  www.w3.org.index.html.svg \
		  www.haskell.org.index.html.svg

