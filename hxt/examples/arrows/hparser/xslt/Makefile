# $Id: Makefile,v 1.1 2006/11/11 15:36:03 hxml Exp $

HXT_HOME	= ../../../..
PKGFLAGS	= 
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

DIST		= $(HXT_HOME)/dist/examples/arrows/hparser
DIST_DIR	= $(DIST)/xslt

prog		= ../HXmlParser
xslt		= $(prog) --do-not-validate

all		:
		$(MAKE) -C .. all

test		:
		@echo "===> run a few simple xslt test cases with $(prog)"
		$(MAKE) test1




testcases	= \
		'strip_example.xsl     strip_example.xml' \
		'elems_and_attrs.xsl   example_document.xml' \
		'lre.xsl               example_document.xml' \
		'for_each_when_if.xsl  example_document.xml' \
		'prio.xsl              example_document.xml' \
		'default_rules.xsl     example_document.xml' \
		'vars_and_params.xsl   example_document.xml' \
		'lre_stylesheet.xsl    example_document.xml' \
		'imp_lre.xsl           example_document.xml' \
		'inc_lre.xsl           example_document.xml' \
		'comment_pi.xsl        example_document.xml' \
		'attset.xsl            example_document.xml' \
		'copy_copy_of.xsl      example_document.xml' \
		'sorting.xsl           example_document.xml' \
		'strip_space.xsl       exdoc2.xml' \
		'testEX.xsl            empty.xml' \
		'testNS.xsl            testNS.xml' \
		'default_prio.xsl      example_document2.xml'

test1		:
		for tc in $(testcases) ; \
		do \
			stylesheet=$$(echo $$tc | cut -d ' ' -f 1) ; \
			input=$$(echo $$tc | cut -d ' ' -f 2) ; \
			$(xslt) --xslt=$$stylesheet --output-file=$$stylesheet.out $$input ; \
			echo "Contents of $$stylesheet.out" ; \
			echo "============================" ; \
			cat $$stylesheet.out ; \
			echo ; \
		done

stylesheets	= $(wildcard *.xsl)
inputs		= $(wildcard *.xml)

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp $(EX) Makefile $(stylesheets) $(inputs) $(DIST_DIR)

clean		:
		rm -f *.xsl.out

