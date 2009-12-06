test	= ../../../RunTestCases

date	:= $(shell date +"%Y-%m-%d-%H:%M:%S")
all	:
	$(MAKE) valid invalid not-wf

valid	: dummy
	cp -f valid.out valid.out.$(date) || true
	cp -f valid.err valid.err.$(date) || true
	( echo 1 ; echo j ) | $(test) ibm_oasis_valid.xml > valid.out 2>valid.err || true
	diff valid.out.$(date) valid.out | tee valid.out.diff

invalid	: dummy
	cp -f invalid.out invalid.out.$(date) || true
	cp -f invalid.err invalid.err.$(date) || true
	( echo 2 ; echo ) | $(test) ibm_oasis_invalid.xml > invalid.out 2>invalid.err || true
	diff invalid.out.$(date) invalid.out | tee invalid.out.diff

not-wf	: dummy
	cp -f not-wf.out not-wf.out.$(date) || true
	cp -f not-wf.err not-wf.err.$(date) || true
	( echo 3 ; echo ) | $(test) ibm_oasis_not-wf.xml > not-wf.out 2>not-wf.err || true
	diff not-wf.out.$(date) not-wf.out | tee not-wf.out.diff

dummy	:
	echo $(date)