# ------------------------------------------------------------
#
# global names and dirs

include ../Version.mk

HXT_HOME	= ..
HXT_HOME_PATH	= $(shell cd $(HXT_HOME) ; pwd)
PACKAGE_DIR	= $(HXT_HOME_PATH)/lib/hxt
HDOC_DIR	= $(HXT_HOME)/doc/hdoc
HDOC_DIR_ARROW	= $(HXT_HOME)/doc/hdoc_arrow
HDOC_DIR_FILTER	= $(HXT_HOME)/doc/hdoc_filter
IMPORT_DIR	= imports
HXT_PACKAGE	= hxt

# ------------------------------------------------------------
#
# commands

GHCFLAGS	= -Wall -O2 -fglasgow-exts -ignore-package $(HXT_PACKAGE)
GHC             = ghc $(GHCFLAGS)
GHCVERSION	=  $(shell ghc -v 2>&1 | head -n 2 | tail -n 1 | cut -d " " -f 5 | sed 's|^.*ghc-\([0-9.]*\)/package.conf|\1|')

HXT_PACKAGE_NAME=$(HXT_PACKAGE)-$(VERSION)

HDOCFLAGS	= -v -v -v -h
HDOC            = haddock $(HDOCFLAGS)

RM              = rm -f

all	:

packagename	:
		@echo $(HXT_PACKAGE_NAME)

# ------------------------------------------------------------
#
# protocol handler config (for removing http or popen)

./Text/XML/HXT/Parser/ProtocolHandler.hs	: ./Text/XML/HXT/ProtocolHandler/ProtocolHandler.hs
		$(MAKE) -C ./Text/XML/HXT/ProtocolHandler default_http

# ------------------------------------------------------------
#
# Unicode Codeblocks and Char Properties generated from Unicode DB

./Text/XML/HXT/RelaxNG/Unicode/Blocks.hs ./Text/XML/HXT/RelaxNG/Unicode/CharProps.hs	:
		$(MAKE) -C ./Text/XML/HXT/RelaxNG/Unicode all

# ------------------------------------------------------------
#
# hxt package variables and rules

HXT_SRC	= \
	Control/Arrow/ArrowIf.hs \
	Control/Arrow/ArrowIO.hs \
	Control/Arrow/ArrowList.hs \
	Control/Arrow/ArrowNF.hs \
	Control/Arrow/ArrowState.hs \
	Control/Arrow/ArrowTree.hs \
	Control/Arrow/IOListArrow.hs \
	Control/Arrow/IOStateListArrow.hs \
	Control/Arrow/ListArrow.hs \
	Control/Arrow/ListArrows.hs \
	Control/Arrow/StateListArrow.hs \
	Data/AssocList.hs \
	Data/Atom.hs \
	Data/Char/UTF8.hs \
	Data/NavTree.hs \
	Data/Tree/Class.hs \
	Data/Tree/NTree/TypeDefs.hs \
	Text/XML/HXT/Arrow/DocumentInput.hs \
	Text/XML/HXT/Arrow/DocumentOutput.hs \
	Text/XML/HXT/Arrow/DTDProcessing.hs \
	Text/XML/HXT/Arrow/Edit.hs \
	Text/XML/HXT/Arrow/GeneralEntitySubstitution.hs \
	Text/XML/HXT/Arrow.hs \
	Text/XML/HXT/Arrow/Namespace.hs \
	Text/XML/HXT/Arrow/ParserInterface.hs \
	Text/XML/HXT/Arrow/Pickle/DTD.hs \
	Text/XML/HXT/Arrow/Pickle.hs \
	Text/XML/HXT/Arrow/Pickle/Schema.hs \
	Text/XML/HXT/Arrow/Pickle/Xml.hs \
	Text/XML/HXT/Arrow/ProcessDocument.hs \
	Text/XML/HXT/Arrow/ReadDocument.hs \
	Text/XML/HXT/Arrow/WriteDocument.hs \
	Text/XML/HXT/Arrow/XmlArrow.hs \
	Text/XML/HXT/Arrow/XmlIOStateArrow.hs \
	Text/XML/HXT/Arrow/XmlRegex.hs \
	Text/XML/HXT/Arrow/XPath.hs \
	Text/XML/HXT/Arrow/XPathSimple.hs \
	Text/XML/HXT/DOM/FormatXmlTree.hs \
	Text/XML/HXT/DOM/Interface.hs \
	Text/XML/HXT/DOM/IsoLatinTables.hs \
	Text/XML/HXT/DOM/MimeTypeDefaults.hs \
	Text/XML/HXT/DOM/MimeTypes.hs \
	Text/XML/HXT/DOM/QualifiedName.hs \
	Text/XML/HXT/DOM/ShowXml.hs \
	Text/XML/HXT/DOM/TypeDefs.hs \
	Text/XML/HXT/DOM/Unicode.hs \
	Text/XML/HXT/DOM/UTF8Decoding.hs \
	Text/XML/HXT/DOM/Util.hs \
	Text/XML/HXT/DOM/XmlKeywords.hs \
	Text/XML/HXT/DOM/XmlNode.hs \
	Text/XML/HXT/DOM/XmlOptions.hs \
	Text/XML/HXT/DTDValidation/AttributeValueValidation.hs \
	Text/XML/HXT/DTDValidation/DocTransformation.hs \
	Text/XML/HXT/DTDValidation/DocValidation.hs \
	Text/XML/HXT/DTDValidation/DTDValidation.hs \
	Text/XML/HXT/DTDValidation/IdValidation.hs \
	Text/XML/HXT/DTDValidation/RE.hs \
	Text/XML/HXT/DTDValidation/TypeDefs.hs \
	Text/XML/HXT/DTDValidation/Validation.hs \
	Text/XML/HXT/DTDValidation/XmlRE.hs \
	Text/XML/HXT/IO/GetFILE.hs \
	Text/XML/HXT/IO/GetHTTPLibCurl.hs \
	Text/XML/HXT/Parser/HtmlParsec.hs \
	Text/XML/HXT/Parser/ProtocolHandlerUtil.hs \
	Text/XML/HXT/Parser/TagSoup.hs \
	Text/XML/HXT/Parser/XhtmlEntities.hs \
	Text/XML/HXT/Parser/XmlCharParser.hs \
	Text/XML/HXT/Parser/XmlDTDParser.hs \
	Text/XML/HXT/Parser/XmlDTDTokenParser.hs \
	Text/XML/HXT/Parser/XmlEntities.hs \
	Text/XML/HXT/Parser/XmlParsec.hs \
	Text/XML/HXT/Parser/XmlTokenParser.hs \
	Text/XML/HXT/RelaxNG/BasicArrows.hs \
	Text/XML/HXT/RelaxNG/CreatePattern.hs \
	Text/XML/HXT/RelaxNG/DataTypeLibMysql.hs \
	Text/XML/HXT/RelaxNG/DataTypeLibraries.hs \
	Text/XML/HXT/RelaxNG/DataTypeLibUtils.hs \
	Text/XML/HXT/RelaxNG/DataTypes.hs \
	Text/XML/HXT/RelaxNG.hs \
	Text/XML/HXT/RelaxNG/PatternFunctions.hs \
	Text/XML/HXT/RelaxNG/PatternToString.hs \
	Text/XML/HXT/RelaxNG/SchemaGrammar.hs \
	Text/XML/HXT/RelaxNG/Schema.hs \
	Text/XML/HXT/RelaxNG/Simplification.hs \
	Text/XML/HXT/RelaxNG/Unicode/Blocks.hs \
	Text/XML/HXT/RelaxNG/Unicode/CharProps.hs \
	Text/XML/HXT/RelaxNG/Utils.hs \
	Text/XML/HXT/RelaxNG/Validation.hs \
	Text/XML/HXT/RelaxNG/Validator.hs \
	Text/XML/HXT/RelaxNG/XmlSchema/DataTypeLibW3C.hs \
	Text/XML/HXT/RelaxNG/XmlSchema/Regex.hs \
        Text/XML/HXT/RelaxNG/XmlSchema/RegexMatch.hs \
	Text/XML/HXT/RelaxNG/XmlSchema/RegexParser.hs \
	Text/XML/HXT/Version.hs \
	Text/XML/HXT/XPath.hs \
	Text/XML/HXT/XPath/NavTree.hs \
	Text/XML/HXT/XPath/XPathArithmetic.hs \
	Text/XML/HXT/XPath/XPathDataTypes.hs \
	Text/XML/HXT/XPath/XPathEval.hs \
	Text/XML/HXT/XPath/XPathFct.hs \
	Text/XML/HXT/XPath/XPathKeywords.hs \
	Text/XML/HXT/XPath/XPathParser.hs \
	Text/XML/HXT/XPath/XPathToNodeSet.hs \
	Text/XML/HXT/XPath/XPathToString.hs


# no longer in use	System/PipeOpen.hs
# no longer in use	Text/XML/HXT/IO/GetHTTPNative.hs
# no longer in use	Text/XML/HXT/IO/GetHTTPCurl.hs

Text/XML/HXT/Version.hs	: ../hxt.cabal
		( echo "module Text.XML.HXT.Version" \
		; echo "where" \
		; echo "hxt_version :: String" \
		; echo "hxt_version = \"$$(cat ../hxt.cabal | grep -i '^version:' | cut -d ' ' -f 2)\"" \
		) > $@

HXT_OBJ		= $(HXT_SRC:.hs=.o)
HXT_HI		= $(HXT_SRC:.hs=.hi)

HXT_DRIVER	= HXT.o
HXT_LIBA	= libHS$(HXT_PACKAGE).a
HXT_LIBO	= HS$(HXT_PACKAGE).o
HXT_LIB		= $(HXT_LIBA) $(HXT_LIBO)
HXT_REQ_PKG	= -package parsec -package curl -package tagsoup

$(HXT_LIBA)	: $(HXT_SRC)
		$(GHC) -package-name $(HXT_PACKAGE_NAME) $(HXT_REQ_PKG) --make $(HXT_DRIVER:.o=.hs)
		ar crs $@ $(HXT_OBJ)
		@echo "#  please note: !! GNU ar (at least 2.15.90) does not set exit code on missing .o files !!"

$(HXT_LIBO)	: $(HXT_LIBA)
		ld -r --whole-archive -o $@ $<

# ------------------------------------------------------------
#


PROFFLAGS	= -Wall -fglasgow-exts $(HXT_REQ_PKG) -prof -auto -ignore-package hxt -ignore-package HTTP -i../profiling

prof		:
		ghc $(PROFFLAGS) --make $(HXT_DRIVER:.o=.hs)

# ------------------------------------------------------------

doc           : $(HXT_SRC)
		[ -d $(HDOC_DIR) ] || mkdir -p $(HDOC_DIR)
		$(HDOC) -t "Haskell Xml Toolbox $(VERSION): The complete API" -o $(HDOC_DIR) $(HXT_SRC)

# ------------------------------------------------------------

DIST		= ../$(DIST_NAME)
DIST_DIR	= $(DIST)/src
DIST_FILES	= Makefile \
		  $(HXT_SRC) $(HXT_DRIVER:.o=.hs) hxt-package.hs \
		  $(wildcard ./Text/XML/HXT/RelaxNG/Unicode/G*.hs) \
		  $(wildcard ./Text/XML/HXT/RelaxNG/Unicode/Makefile) \
		  $(wildcard ./Text/XML/HXT/RelaxNG/Unicode/*.txt)

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		tar cvf - $(DIST_FILES) | ( cd $(DIST_DIR) ; tar xf - )

.PHONY		: all install doc clean dist

# eof ------------------------------------------------------------
