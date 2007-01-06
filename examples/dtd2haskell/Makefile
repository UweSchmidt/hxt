# $Id: Makefile,v 1.5 2005/04/14 12:52:51 hxml Exp $

HXT_HOME	= ../..
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
		$(MAKE) XHTML.o XHTML2.o XHTML3.o
		@echo "===> the generated modules"
		ls -l XHTML*.hs XHTML*.o

EX1		= ../xhtml/xhtml.xml
EX2		= ../photoalbum/photos.xml

XHTML.o		: $(prog) $(EX1)
		@echo "===> generate a module for XHTML access function from the XHTML DTD with naming convention is<Tag>, get<Attr>, ..."
		$(prog) --output-file XHTML.hs --uppercase-initials $(EX1)
		$(GHC) -c XHTML.hs

XHTML2.o	: $(prog) $(EX1)
		@echo "===> generate a module for XHTML access function from the XHTML DTD with naming convention is_<tag>, get_<attr>, ..."
		$(prog) --output-file XHTML2.hs --prefix-underline $(EX1)
		$(GHC) -c XHTML2.hs

XHTML3.o	: $(prog) $(EX1)
		@echo "===> same as test2, but with namespace aware access functions, if xmlns is used in DTD"
		$(prog) --output-file XHTML3.hs --prefix-underline --namespace-aware $(EX1)
		$(GHC) -c XHTML3.hs

Photo.hs	: $(prog) $(EX2)
		$(prog) --output-file $@ --uppercase-initials $(EX2)


DIST		= $(HXT_HOME)/dist/examples
DIST_DIR	= $(DIST)/dtd2haskell
DIST_FILES	= $(prog).hs Makefile

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp $(DIST_FILES) $(DIST_DIR)

clean		:
		rm -f $(prog) XHTML*.hs *.o *.hi

