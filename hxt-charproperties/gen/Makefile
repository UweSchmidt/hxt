CURL = curl --silent
GHC  = stack exec ghc --
#GHC = ghc

target1 = ../src/Data/Char/Properties/UnicodeBlocks.hs
target2 = ../src/Data/Char/Properties/UnicodeCharProps.hs

targets = $(target1) $(target2)

targetDir = $(shell dirname $(target1))

all : $(targets)

new :
	rm -f *.txt
	$(MAKE) all

Blocks.txt	:
	$(CURL) http://www.unicode.org/Public/UNIDATA/Blocks.txt > $@

UnicodeData.txt	:
	$(CURL) http://www.unicode.org/Public/UNIDATA/UnicodeData.txt > $@

$(target1)	: Blocks.txt ./genBlocks
	[ -d $(targetDir) ] || mkdir -p $(targetDir)
	./genBlocks > $@

$(target2)	: UnicodeData.txt ./genCharProps
	[ -d $(targetDir) ] || mkdir -p $(targetDir)
	./genCharProps > $@

./genBlocks	: GenBlocks.hs
	$(GHC) -Wall --make -o $@ $<

./genCharProps	: GenCharProps.hs
	$(GHC) -Wall --make -o $@ $<

clean	:
	rm -f $(targets) ./genBlocks ./genCharProps *.hi *.o

.PHONY	: all new clean
