prog		= dist/build/monad-hparser/monad-hparser

all		: $(prog)

$(prog)		:
		cabal configure && cabal build

force		:
		cabal clean && $(MAKE) $(prog)

test		: $(prog)
		@echo "===> run a few simple test cases with the validating parser"
		@echo "===> first see all command line options"
		$(prog) --help
		@echo
		$(MAKE) test0 test1 test2 test3 test4

EX1		= ../monad-hparser/example1.xml
EX1a		= ../monad-hparser/example1CRLF.xml
EXi		= ../monad-hparser/invalid.xml
EX2		= ../../xhtml/xhtml.xml
EX3		= ../monad-hparser/namespace0.xml
EX3a		= ../monad-hparser/namespace1.xml
EX4		= ../monad-hparser/lousy.html
EX4a		= ../monad-hparser/emptyElements.html

EX		= $(wildcard example*.xml) $(wildcard lousy*.html) $(wildcard empty*.html) $(wildcard *valid*.xml *valid*.rng) $(wildcard namespace*.xml)

test0		:
		@echo "===> a 1. simple valid document"
		$(prog) $(EX1)
		@echo
		@echo "===> the dom tree of the same document (without any redundant whitespace)"
		$(prog) --show-tree --remove-whitespace $(EX1)
		@echo
		@echo "===> the next test case contains validation erors, it must fail"
		$(prog) --verbose $(EXi) || true
		@echo
		@echo "===> same source, but only wellformed check"
		$(prog) --do-not-validate $(EXi) || true
		@echo
		@echo "===> only validation, no output of an XHTML source"
		$(prog) --verbose --no-output $(EX2)
		@echo

test1		:
		@echo "===> the source of a very simple valid document" ; echo ; sleep 2
		cat $(EX1)
		@sleep 2 ; echo ; echo "===> parser will emit UTF-8" ; echo ; sleep 2
		$(prog) --output-encoding=UTF-8 $(EX1)
		@echo
		@sleep 2 ; echo ; echo "===> once again with ISO-8859-1 (latin1) output" ; echo ; sleep 2
		$(prog) --output-encoding=ISO-8859-1 $(EX1)
		@echo
		@sleep 2 ; echo ; echo "===> once again with US-ASCII output" ; echo ; sleep 2
		$(prog) --output-encoding=US-ASCII $(EX1)
		@echo
		@sleep 2 ; echo ; echo "===> once again with hdom tree output" ; echo ; sleep 2
		$(prog) --show-tree --output-encoding=ISO-8859-1 $(EX1)
		@echo
		@sleep 2 ; echo ; echo "===> once again, but without any markup" ; echo ; sleep 2
		$(prog) --action=only-text --output-encoding=ISO-8859-1 $(EX1)
		@echo
		@sleep 2 ; echo ; echo "===> same source, but with CRLF, parser will emit UTF-8" ; echo ; sleep 2
		$(prog) --output-encoding=UTF-8 $(EX1a)
		@echo

test2		:
		@echo "===> the source of a xhtml document" ; echo ; sleep 2
		cat $(EX2)
		@echo "that document has" `cat $(EX2) | wc -l` "lines"
		@sleep 2 ; echo ; echo "===> parser will validate this document and try to indent the output" ; echo ; sleep 2
		$(prog) --indent $(EX2)
		@sleep 2 ; echo ; echo "===> once again, but remove all markup" ; echo ; sleep 2
		$(prog) --action=only-text --remove-whitespace $(EX2)
		@sleep 2 ; echo ; echo "===> once again with hdom tree output" ; echo ; sleep 2
		$(prog) --show-tree --remove-whitespace $(EX2)

test3		:
		@echo "===> namespace processing examples" ; echo ; sleep 2
		@echo "===> namespace propagation test" ; echo ; sleep 2
		$(prog) --verbose --check-namespaces  --indent --output-encoding=UTF-8 $(EX3)
		@echo
		@echo ; sleep 2 ; echo "===> namespace propagation test: tree output with attached namespaces" ; echo ; sleep 2
		$(prog) --verbose --check-namespaces  --remove-whitespace --show-tree --output-encoding=ISO-8859-1 $(EX3)
		@echo
		@echo ; sleep 2 ; echo "===> namespace validation test: this test produces namespace errors" ; echo ; sleep 2
		$(prog) --verbose --do-not-validate --check-namespaces --indent --output-encoding=ISO-8859-1 $(EX3a) || true
		@echo


test4		:
		@echo "===> HTML parsing examples" ; echo ; sleep 2
		@echo "===> the source of a lousy html document" ; echo ; sleep 2
		cat $(EX4)
		@sleep 2 ; echo ; echo "===> parser accepts this document and tries to build a document tree" ; echo ; sleep 2
		$(prog) --indent --preserve-comment --parse-html $(EX4)
		@echo "===> the source of another lousy html document containing empty elements" ; echo ; sleep 2
		cat $(EX4a)
		@sleep 2 ; echo ; echo "===> parser accepts this document and tries to format this as a HTML document without any dangarous empty elements" ; echo ; sleep 2
		$(prog) --indent --preserve-comment --parse-html --output-xhtml $(EX4a)
		@echo

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		cp $(EX) Makefile $(prog).hs $(DIST_DIR)

clean		:
		cabal clean

.PHONY		: all test test0 test1 test2 test3 test4 dist clean prof local force
