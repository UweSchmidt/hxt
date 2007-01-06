# $Id: Makefile,v 1.4 2005/05/15 17:01:04 hxml Exp $

HXT_HOME	= ../../..
PKGFLAGS	= 
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

prog		= DTDtoHXT

all		: $(prog)

$(prog)		: $(prog).hs
		$(GHC) --make -o $@ $<

force		:
		$(GHC) --make -o $(prog) $(prog).hs

test		:
		@echo "===> run a few generation examples"
		$(MAKE) XHTML.o XHTML2.o
		@echo "===> the generated modules"
		ls -l XHTML*.hs XHTML*.o

EX1		= ../../xhtml/xhtml.xml
EX2		= ../../photoalbum/photos.xml

XHTML.o		: $(prog) $(EX1)
		@echo "===> generate a module for XHTML access function from the XHTML DTD with naming convention is<Elem>, get<Attr>, ..."
		$(prog) --output-file XHTML.hs --uppercase-initials $(EX1)
		$(GHC) -c XHTML.hs

XHTML2.o	: $(prog) $(EX1)
		@echo "===> generate a module for XHTML access function from the XHTML DTD with naming convention is_<elem>, get_<attr>, ..."
		$(prog) --output-file XHTML2.hs --prefix-underline $(EX1)
		$(GHC) -c XHTML2.hs

Photo.hs	: $(prog) $(EX2)
		$(prog) --output-file $@ --uppercase-initials $(EX2)


DIST		= $(HXT_HOME)/dist/examples/arrows
DIST_DIR	= $(DIST)/dtd2hxt
DIST_FILES	= $(prog).hs Makefile

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp $(DIST_FILES) $(DIST_DIR)

clean		:
		rm -f $(prog) XHTML*.hs *.o *.hi

