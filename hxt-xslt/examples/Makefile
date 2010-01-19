# Example Applications
#

EXAMPLES	= hparser
MORE_SAMPLES	= 

all		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i all ;)

test		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)

clean		:
		$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)

.PHONY		: all test clean

