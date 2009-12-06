# ------------------------------------------------------------
#
# global names and dirs

HXT_HOME	= ../../../
HXT_HOME_PATH	= $(shell cd $(HXT_HOME) ; pwd)
HXT_SRC_DIR	= $(HXT_HOME_PATH)/src

GHC_FLAGS	= -package hxt -Wall
GHC		= ghc $(GHC_FLAGS)
RELAX_PATH	= $(HXT_SRC_DIR)/Text/XML/HXT/RelaxNG
THIS_DIR	= $(shell pwd)

# ------------------------------------------------------------
#
# create a new Haskell Modul of the Relax NG grammar schema
# Schema.hs is used to validate a Relax NG schema and the content of a
# schema referenced by the ref-pattern.
# SchemaGrammar.hs is used to validate the content of a schema
# referenced by the include-pattern

targets		= Schema.hs SchemaGrammar.hs

all		: $(targets)

install		: $(targets)
		cp $(targets) ..

clean		:
		rm -rf $(targets) RelaxSchemaToXmlTree *.o *.hi

RelaxSchemaToXmlTree	: RelaxSchemaToXmlTree.hs
		$(GHC) --make -o $@ $<

SchemaGrammar.hs: SchemaGrammar.rng RelaxSchemaToXmlTree
		./RelaxSchemaToXmlTree $(basename $@)

Schema.hs	: Schema.rng RelaxSchemaToXmlTree
		./RelaxSchemaToXmlTree $(basename $@)

# eof ------------------------------------------------------------
