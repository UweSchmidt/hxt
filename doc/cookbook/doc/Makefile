# $Id: Makefile,v 1.2 2005/09/30 14:41:43 hxml Exp $

latex	= unset TEXINPUTS ; latex

images	= images/blankNode.eps  images/hxt-structure.eps     images/simple_graph2.eps  images/simplegraph1.eps  images/typedL2.eps \
	images/fhlogo.eps     images/module_hierachie.eps  images/simple_graph3.eps  images/simplegraph2.eps  images/typedL3.eps \
	images/simple_Graph4.eps     images/simple_graph5.eps  images/typedL.eps        images/typedLiteral.eps 

all	: thesis.pdf

thesis.dvi	: $(images)

$(images) :
	$(MAKE) -C images all

clean	:
	$(MAKE) -C images clean
	rm -f thesis.dvi thesis.lof thesis.lol thesis.toc thesis.aux thesis.log thesis.ps

reallyClean	:
	$(MAKE) clean
	rm -f thesis.pdf

%.pdf	: %.dvi
	dvipdf $<

%.dvi	: %.tex
	$(latex) $<
	$(latex) $<
	$(latex) $<
	$(latex) $<

