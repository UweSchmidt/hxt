# $Id: Makefile,v 1.4 2004/09/02 19:11:54 hxml Exp $

HXT_HOME	= ../..
PACKAGE_DIR	= $(HXT_HOME)/lib/hxt
GHC		= ghc -Wall -O -package-conf $(PACKAGE_DIR)/package.conf -package hxt

prog		= ./RunTestCases

all		: $(prog)

RunTestCases	: RunTestCases.hs
		$(GHC) --make -o $@ $<

force		:
		$(GHC) --make -o $(prog) $(prog).hs

date		:= $(shell date +%Y-%m-%d.%H.%M.%S)

testDir		= .
testSuite	= $(testDir)/xml-test-suite/xmlconf/xmlconf.xml

OPT		=

test	: RunTestCases
	RunTestCases -h
	RunTestCases -v $(OPT) --only-err $(testSuite) 2> w3c.err | tee w3c.out
	cp w3c.err w3c.err.$(date)
	cp w3c.out w3c.out.$(date)


valid	:
	RunTestCases -v $(OPT) --test-type=valid $(testSuite)

invalid	:
	RunTestCases -v $(OPT) --test-type=invalid $(testSuite)

not-wf	:
	RunTestCases -v $(OPT) --test-type=not-wf $(testSuite)

error	:
	RunTestCases -v $(OPT) --test-type=error $(testSuite)


unpack	:
	tar xvzf xml-test-suite.tgz

clean	:
	rm -f $(prog)

