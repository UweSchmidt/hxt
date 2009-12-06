# $Id: Makefile,v 1.1 2005/09/05 15:20:25 hxml Exp $

images	= blankNode.eps fhlogo.eps hxt-structure.eps module_hierachie.eps simple_Graph4.eps simple_graph2.eps \
	simple_graph3.eps simple_graph5.eps simplegraph1.eps simplegraph2.eps typedL.eps typedL2.eps typedL3.eps typedLiteral.eps

all	: $(images)

clean	:
	rm -f *.eps

%.eps	: %.png
	convert $< $@

%.eps	: %.tif
	convert $< $@

