
target1 = ./Blocks.hs
target2 = ./CharProps.hs

targets = $(target1) $(target2)

targetDir = $(shell dirname $(target1))

all : $(targets)

Blocks.txt	:
	wget http://www.unicode.org/Public/UNIDATA/Blocks.txt

UnicodeData.txt	:
	wget http://www.unicode.org/Public/UNIDATA/UnicodeData.txt

$(target1)	: Blocks.txt ./genBlocks
	[ -d $(targetDir) ] || mkdir -p $(targetDir)
	./genBlocks > $@

$(target2)	: UnicodeData.txt ./genCharProps
	[ -d $(targetDir) ] || mkdir -p $(targetDir)
	./genCharProps > $@

./genBlocks	: GenBlocks.hs
	ghc -Wall --make -o $@ $<

./genCharProps	: GenCharProps.hs
	ghc -Wall --make -o $@ $<

clean	:
	rm -f $(targets) ./genBlocks ./genCharProps
