# ------------------------------------------------------------
#
# global names and dirs

include ../Version.mk

HXT_HOME	= ..
HXT_HOME_PATH	= $(shell cd $(HXT_HOME) ; pwd)
PACKAGE_DIR	= $(HXT_HOME_PATH)/lib/$(SOFTWARE)
IMPORT_DIR	= imports
HXT_PACKAGE	= $(SOFTWARE)

# ------------------------------------------------------------
#
# commands

GHCFLAGS	= -Wall -O2 -fglasgow-exts -ignore-package $(SOFTWARE)
GHC             = ghc $(GHCFLAGS)
GHCVERSION	=  $(shell ghc -v 2>&1 | head -n 2 | tail -n 1 | cut -d " " -f 5 | sed 's|^.*ghc-\([0-9.]*\)/package.conf|\1|')

HXT_PACKAGE_NAME=$(DIST_NAME)

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
# hxt package variables and rules

HXT_SRC	= \
	$(wildcard Control/Monad/*.hs) \
        $(wildcard System/*.hs) \
	$(wildcard Text/XML/HXT/*.hs) \
	$(wildcard Text/XML/HXT/DOM/*.hs) \
	$(wildcard Text/XML/HXT/Parser/*.hs) \
	$(wildcard Text/XML/HXT/Validator/*.hs)

HXT_OBJ		= $(HXT_SRC:.hs=.o)
HXT_HI		= $(HXT_SRC:.hs=.hi)

HXT_DRIVER	= HXT.o
HXT_LIBA	= libHS$(HXT_PACKAGE).a
HXT_LIBO	= HS$(HXT_PACKAGE).o
HXT_LIB		= $(HXT_LIBA) $(HXT_LIBO)
HXT_REQ_PKG	= -package parsec -package HTTP -package hxt-8.3.0

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

LOCAL_INSTALL_DIR	= $(PACKAGE_DIR)
LOCAL_IMPORT_DIR	= $(PACKAGE_DIR)/$(IMPORT_DIR)

PKG_LIB		= 
PKG_NAME	=
PKG_HI		=

install_local_$(HXT_PACKAGE)	: $(HXT_LIB) $(SOFTWARE)-package.conf
		$(MAKE) install_local_package PKG_NAME=$(HXT_PACKAGE) PKG_LIB="$(HXT_LIB)" PKG_HI="$(HXT_HI)"

install_local_package	: $(PKG_NAME)-package.conf
		[ -d $(LOCAL_INSTALL_DIR) ] || mkdir -p $(LOCAL_INSTALL_DIR)
		[ -d $(LOCAL_IMPORT_DIR)  ] || mkdir -p $(LOCAL_IMPORT_DIR)
		cp -f  $(PKG_LIB)    $(LOCAL_INSTALL_DIR)
		tar cvf - $(PKG_HI) | ( cd $(LOCAL_IMPORT_DIR) ; tar xf - )
		[ -f $(LOCAL_INSTALL_DIR)/package.conf ] || echo "[]" > $(LOCAL_INSTALL_DIR)/package.conf
		cat $(PKG_NAME)-package.conf \
		| ghc-pkg update - --package-conf=$(LOCAL_INSTALL_DIR)/package.conf
		@echo use package with: ghc\(i\) ... -package-conf $(LOCAL_INSTALL_DIR)/package.conf -package $(PKG_NAME) ...

GHC_INSTALL	= $(shell ghc -v 2>&1 | head -n 2 | tail -n 1 | cut -d " " -f 5 | sed 's|/package.conf||')
GHC_INSTALL_DIR	= $(GHC_INSTALL)/$(SOFTWARE)
GHC_PKG_CONF	=

install		: $(SOFTWARE)-package.conf
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

$(SOFTWARE)-package.conf	: $(SOFTWARE)-package.hs
		ghc -o $(SOFTWARE)-package $(SOFTWARE)-package.hs
		./$(SOFTWARE)-package $(VERSION) "ghc" $(LOCAL_INSTALL_DIR) $(HXT_SRC) > $@

../$(SOFTWARE).cabal	: $(SOFTWARE)-package.hs Makefile ../Makefile
		ghc -o $(SOFTWARE)-package $(SOFTWARE)-package.hs
		./$(SOFTWARE)-package $(VERSION) "cabal" $(GHC_INSTALL_DIR) $(HXT_SRC) > $@

install_packages	:
		runghc $(SOFTWARE)-package.hs $(VERSION) "ghc" $(GHC_INSTALL_DIR) $(HXT_SRC) \
		| ghc-pkg --update-package

uninstall_packages	:
		ghc-pkg --remove-package $(HXT_PACKAGE) || true

clean_packages	:
		$(MAKE) clean_$(HXT_PACKAGE)

clean		:
		$(MAKE) clean_packages
		$(MAKE) -C ./Text/XML/HXT/ProtocolHandler clean
		rm -rf $(LOCAL_INSTALL_DIR) $(SOFTWARE)-package.conf ../$(SOFTWARE).cabal

clean_$(HXT_PACKAGE)	:
		$(RM) $(HXT_DRIVER) $(HXT_DRIVER:.o=.hi) $(HXT_OBJ) $(HXT_HI) $(HXT_LIB)

# ------------------------------------------------------------

DIST		= ../$(DIST_NAME)
DIST_DIR	= $(DIST)/src
DIST_FILES	= Makefile \
		  $(HXT_SRC) $(HXT_DRIVER:.o=.hs) $(SOFTWARE)-package.hs \
		  ./Text/XML/HXT/ProtocolHandler/ProtocolHandler.hs \
		  ./Text/XML/HXT/ProtocolHandler/Makefile


dist		:
		[ -d $(DIST_DIR) ] || mkdir -p $(DIST_DIR)
		tar cvf - $(DIST_FILES) | ( cd $(DIST_DIR) ; tar xf - )

.PHONY		: all install doc clean dist

# eof ------------------------------------------------------------
