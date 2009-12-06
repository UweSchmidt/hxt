# $Id: Makefile,v 1.5 2002/09/01 20:32:01 hxml1 Exp $

# makes html pdf and postscript output
all:	html pdf


cover:
	latex cover_page.tex
	xdvi cover_page.dvi

coverps:
	latex cover_page.tex
	dvips cover_page.dvi -o cover_page.ps


html:
	make cleanhtml
	db2html --dsl thesis.dsl\#html thesis.xml
	mkdir thesis/images
	cp images/*.gif thesis/images

pdf:
	make ps
	ps2pdf thesis.ps

ps:
	db2ps --dsl thesis.dsl\#print thesis.xml

rtf:
	db2rtf thesis.xml

txt:
	docbook2txt thesis.xml

clean:	cleanhtml
	rm -f thesis.aux \
	      thesis.fot \
	      thesis.log \
	      thesis.out \
	      thesis.ps \
	      thesis.rtf \
	      thesis.txt \
	      cover_page.aux \
	      cover_page.dvi \
	      cover_page.ps \
	      cover_page.log

reallyClean	:
	$(MAKE) clean
	rm -f thesis.pdf cover_page.pdf

cleanhtml:
	rm -Rf thesis
