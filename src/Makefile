# ------------------------------------------------------------
#
# global names and dirs

VERSION		= 7.4
HXT_HOME	= ..
HXT_HOME_PATH	= $(shell cd $(HXT_HOME) ; pwd)
PACKAGE_DIR	= $(HXT_HOME_PATH)/lib/hxt
HDOC_DIR	= $(HXT_HOME)/doc/hdoc
HDOC_DIR_ARROW	= $(HXT_HOME)/doc/hdoc_arrow
HDOC_DIR_FILTER	= $(HXT_HOME)/doc/hdoc_filter
HDOC_FLAGS	= --source=http://darcs.fh-wedel.de/hxt/src/%F --package=hxt-$(VERSION)
IMPORT_DIR	= imports
HXT_PACKAGE	= hxt

# ------------------------------------------------------------
#
# commands

GHCFLAGS	= -Wall -O2 -fglasgow-exts -ignore-package $(HXT_PACKAGE)
GHC             = ghc $(GHCFLAGS)
GHCVERSION	=  $(shell ghc -v 2>&1 | head -n 2 | tail -n 1 | cut -d " " -f 5 | sed 's|^.*ghc-\([0-9.]*\)/package.conf|\1|')

HXT_PACKAGE_NAME=$(HXT_PACKAGE)-$(VERSION)

HDOCFLAGS	= -v -h $(HDOC_FLAGS) --use-package=base
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

HXT_SRC		= $(wildcard ./Data/*.hs) \
		  $(wildcard ./Data/Char/*.hs) \
		  $(wildcard ./Data/Tree/*.hs) \
		  $(wildcard ./Data/Tree/NTree/*.hs) \
		  $(wildcard ./Control/Arrow/*.hs) \
		  $(wildcard ./Control/Monad/*.hs) \
		  $(wildcard ./Control/Strategies/*.hs) \
		  $(wildcard ./System/*.hs) \
		  $(wildcard ./Text/XML/HXT/[DPX]*.hs) \
		  $(wildcard ./Text/XML/HXT/DOM/*.hs) \
		  $(wildcard ./Text/XML/HXT/Parser/*.hs) \
		  $(wildcard ./Text/XML/HXT/Validator/*.hs) \
		  $(wildcard ./Text/XML/HXT/XPath/*.hs) \
		  $(wildcard ./Text/XML/HXT/Arrow.hs) \
		  $(wildcard ./Text/XML/HXT/Arrow/[DEGNPRWX]*.hs) \
		  $(wildcard ./Text/XML/HXT/Arrow/Pickle/*.hs) \
		  $(wildcard ./Text/XML/HXT/IO/*.hs) \
		  $(wildcard ./Text/XML/HXT/RelaxNG.hs) \
		  $(wildcard ./Text/XML/HXT/RelaxNG/*.hs) \
		  $(wildcard ./Text/XML/HXT/RelaxNG/Unicode/[BC]*.hs) \
		  $(wildcard ./Text/XML/HXT/RelaxNG/XmlSchema/*.hs) \
		  $(wildcard ./Text/XML/HXT/XSLT/[ACNX]*.hs)

HXT_SRC_FILTER	= $(wildcard ./Data/*.hs) \
		  $(wildcard ./Data/Char/*.hs) \
		  $(wildcard ./Data/Tree/*.hs) \
		  $(wildcard ./Data/Tree/NTree/*.hs) \
		  $(wildcard ./Control/Monad/*.hs) \
		  $(wildcard ./Control/Strategies/*.hs) \
		  $(wildcard ./System/*.hs) \
		  $(wildcard ./Text/XML/HXT/[DIPX]*.hs) \
		  $(wildcard ./Text/XML/HXT/DOM/*.hs) \
		  $(wildcard ./Text/XML/HXT/Parser/*.hs) \
		  $(wildcard ./Text/XML/HXT/Validator/*.hs) \
		  $(wildcard ./Text/XML/HXT/XPath/*.hs) \

HXT_SRC_ARROW	= $(wildcard ./Data/*.hs) \
		  $(wildcard ./Data/Char/*.hs) \
		  $(wildcard ./Data/Tree/*.hs) \
		  $(wildcard ./Data/Tree/NTree/T*.hs) \
		  $(wildcard ./Control/Arrow/*.hs) \
		  $(wildcard ./Control/Monad/*.hs) \
		  $(wildcard ./Control/Strategies/*.hs) \
		  $(wildcard ./Text/XML/HXT/[A]*.hs) \
		  $(wildcard ./Text/XML/HXT/DOM/NamespacePredicates.hs) \
		  $(wildcard ./Text/XML/HXT/DOM/[ITU]*.hs) \
		  $(wildcard ./Text/XML/HXT/DOM/XmlKeywords.hs) \
		  $(wildcard ./Text/XML/HXT/Parser/XmlOptions.hs) \
		  $(wildcard ./Text/XML/HXT/Arrow.hs) \
		  $(wildcard ./Text/XML/HXT/Arrow/[DEGNPRWX]*.hs) \
		  $(wildcard ./Text/XML/HXT/Arrow/Pickle/*.hs) \
		  $(wildcard ./Text/XML/HXT/RelaxNG/[DSV]*.hs) \
		  $(wildcard ./Text/XML/HXT/RelaxNG/Unicode/[BC]*.hs) \
		  $(wildcard ./Text/XML/HXT/RelaxNG/XmlSchema/*.hs) \
		  $(wildcard ./Text/XML/HXT/XSLT/[ACNX]*.hs)


# HXT_OBJ		= $(patsubst %.lhs,%.o,$(patsubst %.hs,%.o,$(HXT_SRC)))		# no .lhs sources
# HXT_HI		= $(patsubst %.lhs,%.hi,$(patsubst %.hs,%.hi,$(HXT_SRC)))

HXT_OBJ		= $(HXT_SRC:.hs=.o)
HXT_HI		= $(HXT_SRC:.hs=.hi)

HXT_DRIVER	= HXT.o
HXT_LIBA	= libHS$(HXT_PACKAGE).a
HXT_LIBO	= HS$(HXT_PACKAGE).o
HXT_LIB		= $(HXT_LIBA) $(HXT_LIBO)
HXT_REQ_PKG	= -package parsec -package HTTP

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
#
# under construction: XSLT

XSLT_SRC	= $(wildcard ./Text/XML/HXT/XSLT/*.hs)
XSLT_DRIVER	= ./Text/XML/HXT/XSLT/XSLT.o

xslt		: $(XSLT_DRIVER)

$(XSLT_DRIVER)	: $(XSLT_SRC)
		$(GHC) -package-name $(HXT_PACKAGE) $(HXT_REQ_PKG) --make $*

# ------------------------------------------------------------

LOCAL_INSTALL_DIR	= $(PACKAGE_DIR)
LOCAL_IMPORT_DIR	= $(PACKAGE_DIR)/$(IMPORT_DIR)

PKG_LIB		= 
PKG_NAME	=
PKG_HI		=

install_local_$(HXT_PACKAGE)	: $(HXT_LIB) hxt-package.conf
		$(MAKE) install_local_package PKG_NAME=$(HXT_PACKAGE) PKG_LIB="$(HXT_LIB)" PKG_HI="$(HXT_HI)"

install_local_package	: $(PKG_NAME)-package.conf
		[ -d $(LOCAL_INSTALL_DIR) ] || mkdir -p $(LOCAL_INSTALL_DIR)
		[ -d $(LOCAL_IMPORT_DIR)  ] || mkdir -p $(LOCAL_IMPORT_DIR)
		cp -f  $(PKG_LIB)    $(LOCAL_INSTALL_DIR)
		tar cvf - $(PKG_HI) | ( cd $(LOCAL_IMPORT_DIR) ; tar xf - )
		[ -f $(LOCAL_INSTALL_DIR)/package.conf ] || echo "[]" > $(LOCAL_INSTALL_DIR)/package.conf
		cat $(PKG_NAME)-package.conf \
		| ghc-pkg --update-package --config-file $(LOCAL_INSTALL_DIR)/package.conf
		@echo use package with: ghc\(i\) ... -package-conf $(LOCAL_INSTALL_DIR)/package.conf -package $(PKG_NAME) ...

GHC_INSTALL	= $(shell ghc -v 2>&1 | head -n 2 | tail -n 1 | cut -d " " -f 5 | sed 's|/package.conf||')
GHC_INSTALL_DIR	= $(GHC_INSTALL)/hxt
GHC_PKG_CONF	=

install		: hxt-package.conf
		@[ -d $(GHC_INSTALL)     ] || ( echo "ghc install dir not found"       1>&2 ; exit 1 )
		@[ -w $(GHC_INSTALL)     ] || ( echo "you need root access for this target" ; exit 1 )
		[ -d $(GHC_INSTALL_DIR) ] || mkdir -p $(GHC_INSTALL_DIR)
		cp -r $(LOCAL_INSTALL_DIR)/*.[ao] $(LOCAL_INSTALL_DIR)/imports $(GHC_INSTALL_DIR)
		$(MAKE) install_packages
		@echo
		@echo use package with: ghc\(i\) ... -package $(HXT_PACKAGE) ...

uninstall	:
		@[ -d $(GHC_INSTALL) ] || ( echo "ghc install dir not found"       1>&2 ; exit 1 )
		@[ -w $(GHC_INSTALL) ] || ( echo "you need root access for this target" ; exit 1 )
		$(MAKE) uninstall_packages
		rm -rf $(GHC_INSTALL_DIR)

# ------------------------------------------------------------

all		:
		$(MAKE) install_local_$(HXT_PACKAGE)

hxt-package.conf	: hxt-package.hs
		runghc hxt-package.hs $(VERSION) "ghc" $(LOCAL_INSTALL_DIR) $(HXT_SRC) > $@

../hxt.cabal	: hxt-package.hs Makefile ../Makefile
		runghc hxt-package.hs $(VERSION) "cabal" $(GHC_INSTALL_DIR) $(HXT_SRC) > $@

install_packages	:
		runghc hxt-package.hs $(VERSION) "ghc" $(GHC_INSTALL_DIR) $(HXT_SRC) \
		| ghc-pkg --update-package

uninstall_packages	:
		ghc-pkg --remove-package $(HXT_PACKAGE) || true

clean_packages	:
		$(MAKE) clean_$(HXT_PACKAGE)

doc		: $(HXT_SRC)
		[ -d $(HDOC_DIR) ] || mkdir -p $(HDOC_DIR)
		$(HDOC) -t "Haskell Xml Toolbox $(VERSION): The complete API" -o $(HDOC_DIR) $(HXT_SRC)

doc_arrow	: $(HXT_SRC_ARROW)
		[ -d $(HDOC_DIR_ARROW) ] || mkdir -p $(HDOC_DIR_ARROW)
		$(HDOC) -t "Haskell Xml Toolbox $(VERSION): The Arrow API" -o $(HDOC_DIR_ARROW) $(HXT_SRC_ARROW)

doc_filter	: $(HXT_SRC_FILTER)
		[ -d $(HDOC_DIR_FILTER) ] || mkdir -p $(HDOC_DIR_FILTER)
		$(HDOC) -t "Haskell Xml Toolbox $(VERSION): The Filter API" -o $(HDOC_DIR_FILTER) $(HXT_SRC_FILTER)

clean		:
		$(MAKE) clean_packages
		$(MAKE) -C ./Text/XML/HXT/ProtocolHandler clean
		rm -rf $(LOCAL_INSTALL_DIR)

clean_$(HXT_PACKAGE)	:
		$(RM) $(HXT_DRIVER) $(HXT_DRIVER:.o=.hi) $(HXT_OBJ) $(HXT_HI) $(HXT_LIB)

# ------------------------------------------------------------

DIST		= $(HXT_HOME)/dist
DIST_DIR	= $(DIST)/src
DIST_FILES	= Makefile \
		  $(HXT_SRC) $(HXT_DRIVER:.o=.hs) hxt-package.hs \
		  ./Text/XML/HXT/ProtocolHandler/ProtocolHandler.hs ./Text/XML/HXT/ProtocolHandler/Makefile \
		  $(wildcard ./Text/XML/HXT/RelaxNG/Unicode/G*.hs) \
		  $(wildcard ./Text/XML/HXT/RelaxNG/Unicode/Makefile) \
		  $(wildcard ./Text/XML/HXT/RelaxNG/Unicode/*.txt)

dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		tar cvf - $(DIST_FILES) | ( cd $(DIST_DIR) ; tar xf - )

.PHONY		: all install doc clean dist

# eof ------------------------------------------------------------
