
# suck iso-latin-??? encoding tables from unicode.org

all		: DecodeLatin.hs

DecodeLatin.hs	: GenDecodeLatin
		./$<

GenDecodeLatin	: GenDecodeLatin.hs
		ghc --make -o $@ $<

getMappings	:
		wget -v -r ftp://ftp.unicode.org/Public/MAPPINGS/ISO8859/
