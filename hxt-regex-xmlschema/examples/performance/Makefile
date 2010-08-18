PKGFLAGS	=
GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

CNT		= 3

ropts		= +RTS -s -RTS

prog		= ./REtest
prog2		= ./Lines
prog3		= ./RElines
prog3a		= ./SElines
prog4		= ./Words
prog5		= ./REwords
prog5a		= ./SEwords
prog0		= ./Copy

progs		= $(prog) $(prog0) $(prog2) $(prog3) $(prog3a) $(prog4) $(prog5) $(prog5a)

all		: $(progs)

$(prog)		: $(prog).hs
		$(GHC) --make -o $@ $<

local		:
		$(GHC) --make -o $(prog) -fglasgow-exts $(prog).hs

$(prog2)	: $(prog)
		ln -f $(prog) $(prog2)

$(prog3)	: $(prog)
		ln -f $(prog) $(prog3)

$(prog3a)	: $(prog)
		ln -f $(prog) $(prog3a)

$(prog4)	: $(prog)
		ln -f $(prog) $(prog4)

$(prog5)	: $(prog)
		ln -f $(prog) $(prog5)

$(prog5a)	: $(prog)
		ln -f $(prog) $(prog5a)

$(prog0)	: $(prog)
		ln -f $(prog) $(prog0)

# generate and read documents containing text
# with 2^i characters

tests		= 25

test		: $(prog)
		$(MAKE) genfiles   tests="$(tests)"
		$(MAKE) copy       tests="$(tests)"
		$(MAKE) lines      tests="$(tests)"
		$(MAKE) relines    tests="$(tests)"
		$(MAKE) selines    tests="$(tests)"
		$(MAKE) words      tests="$(tests)"
		$(MAKE) rewords    tests="$(tests)"
		$(MAKE) sewords    tests="$(tests)"

perftest	: $(prog)
		$(MAKE) test tests="2 3 10 11 12 13 14 15 16 17 18 19 20"

genfiles	:
		@for i in $(tests) ; \
		do \
		echo time $(prog) $(ropts) $$i ; \
		time $(prog) $(ropts) $$i ; \
		ls -l lines-*$$i.txt ; \
		echo ; \
		done

copy	:
		@for i in $(tests) ; \
		do \
		echo time $(prog0) $(ropts) $$i ; \
		time $(prog0) $(ropts) $$i ; \
		ls -l lines-*$$i.txt.copy ; \
		echo ; \
		done

lines	:
		@for i in $(tests) ; \
		do \
		echo time $(prog2) $(ropts) $$i ; \
		time $(prog2) $(ropts) $$i ; \
		ls -l lines-*$$i.txt.lines ; \
		echo ; \
		done

relines	:
		@for i in $(tests) ; \
		do \
		echo time $(prog3) $(ropts) $$i ; \
		time $(prog3) $(ropts) $$i ; \
		ls -l lines-*$$i.txt.relines ; \
		echo ; \
		done

selines	:
		@for i in $(tests) ; \
		do \
		echo time $(prog3a) $(ropts) $$i ; \
		time $(prog3a) $(ropts) $$i ; \
		ls -l lines-*$$i.txt.selines ; \
		echo ; \
		done

words	:
		@for i in $(tests) ; \
		do \
		echo time $(prog4) $(ropts) $$i ; \
		time $(prog4) $(ropts) $$i ; \
		ls -l lines-*$$i.txt.words ; \
		echo ; \
		done

rewords	:
		@for i in $(tests) ; \
		do \
		echo time $(prog5) $$i ; \
		time $(prog5) $(ropts) $$i ; \
		ls -l lines-*$$i.txt.rewords ; \
		echo ; \
		done

sewords	:
		@for i in $(tests) ; \
		do \
		echo time $(prog5a) $$i ; \
		time $(prog5a) $(ropts) $$i ; \
		ls -l lines-*$$i.txt.sewords ; \
		echo ; \
		done

distclean	: clean

clean		:
		rm -f $(progs) *.o *.hi *.txt *.txt.*

.PHONY		: clean distclean test perftest all local words rewords sewords lines relines selines copy genfiles

