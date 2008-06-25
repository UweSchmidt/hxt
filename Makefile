SOFTWARE	= janus
VERSION		= 1.1.0
VERSIONTAG	= $(SOFTWARE)-$(VERSION)

HADDOCK		= haddock
HC      	= ghc
HC_OPTS 	= -Wall -fglasgow-exts \
			-fno-warn-duplicate-exports \
			-fno-warn-deprecations \
			-fno-warn-overlapping-patterns \
			-farrows \
			-ibuild:src -odir build -hidir build

SRCBASE		= src/Network/Server/Janus
BUILDBASE	= build/Network/Server/Janus
DOCBASE		= temp/Network/Server/Janus

SRCS 		= $(SRCS_BASE) $(SRCS_HANDLER) $(SRCS_SHADER)
SRCS_BASE	= $(SRCBASE)/ContextBrowser.hs \
		  $(SRCBASE)/DynamicLoader.hs \
		  $(SRCBASE)/HTMLBuilder.hs $(SRCBASE)/Messaging.hs $(SRCBASE)/Server.hs \
		  $(SRCBASE)/Core.hs \
		  $(SRCBASE)/ShopExample.hs $(SRCBASE)/Transaction.hs \
		  $(SRCBASE)/XmlHelper.hs $(SRCBASE)/JanusPaths.hs

SRCS_HANDLER 	= $(SRCBASE)/Handler/TCPHandler.hs $(SRCBASE)/Handler/ConsoleHandler.hs
SRCS_SHADER 	= $(SRCBASE)/Shader/ControlShader.hs $(SRCBASE)/Shader/DaemonShader.hs \
		  $(SRCBASE)/Shader/ExprShader.hs $(SRCBASE)/Shader/HTTPShader.hs \
		  $(SRCBASE)/Shader/PredicateShader.hs $(SRCBASE)/Shader/ServletShader.hs \
	          $(SRCBASE)/Shader/ShaderLib.hs $(SRCBASE)/Shader/SOAPShader.hs $(SRCBASE)/Shader/ConsoleShader.hs

OBJS 		= $(OBJS_BASE) $(OBJS_HANDLER) $(OBJS_SHADER)
OBJS_BASE	= $(BUILDBASE)/Core.o $(BUILDBASE)/XmlHelper.o $(BUILDBASE)/JanusPaths.o \
		  $(BUILDBASE)/Transaction.o $(BUILDBASE)/Messaging.o $(BUILDBASE)/DynamicLoader.o $(BUILDBASE)/HTMLBuilder.o
OBJS_HANDLER 	= $(BUILDBASE)/Handler/TCPHandler.o $(BUILDBASE)/Handler/ConsoleHandler.o
OBJS_SHADER 	= $(BUILDBASE)/Shader/SystemShader.o $(BUILDBASE)/Shader/ControlShader.o \
		  $(BUILDBASE)/Shader/DaemonShader.o $(BUILDBASE)/Shader/ExprShader.o \
		  $(BUILDBASE)/Shader/HTTPShader.o $(BUILDBASE)/Shader/PredicateShader.o \
		  $(BUILDBASE)/Shader/ServletShader.o $(BUILDBASE)/Shader/ShaderLib.o \
		  $(BUILDBASE)/Shader/ConsoleShader.o $(BUILDBASE)/Shader/TestShader.o

OBJS_EXAMPLES	= $(BUILDBASE)/ContextBrowser.o  $(BUILDBASE)/ShopExample.o
# $(BUILDBASE)/Shader/SOAPShader.o

OBJS_SERVER	= $(OBJS_HANDLER) $(OBJS_SHADER) $(OBJS_EXAMPLES)

DOC_SRCS_1	= $(subst $(BUILDBASE),$(DOCBASE),$(OBJS))
DOC_SRCS	= $(patsubst %.o,%.hs,$(DOC_SRCS_1))

build/%.o: 	src/%.hs
		$(HC) -c $< $(HC_OPTS)

%.hi: 		%.o
		@:

all:		janus doc

janus:		./build ./build/conf ./build/wwwpages ./build/errorpages $(OBJS_SERVER)
		$(HC) $(HC_OPTS) --make -o build/janus $(SRCBASE)/Janus.hs
		$(MAKE) pages

pages	:
		cp conf/* build/conf
		cp -r wwwpages/* build/wwwpages
		cp errorpages/* build/errorpages

build:
		mkdir build

build/conf:
		mkdir build/conf

build/wwwpages:
		mkdir build/wwwpages

build/errorpages:
		mkdir build/errorpages

doc:		$(DOC_SRCS)
		mkdir -p ./doc
		$(HADDOCK) --html --odir=./doc $(DOC_SRCS)
		rm ./temp/* -R
		rmdir ./temp

temp/%.hs: 	src/%.hs
		mkdir -p $(@D)
		cat $< | sed -e 's/proc /\\/g' > $@

depend:
		ghc -M $(HC_OPTS) $(SRCS)

clean:
		rm -f build -r

tag		:
		darcs tag $(VERSIONTAG) .

# ------------------------------------------------------------

PLUGINDIR	= janus-plugins
LIBDIR		= janus-library
SRVDIR		= janus-server
TESTDIR		= test

janus-lib	:
		( cd $(LIBDIR) \
		; runhaskell Setup.hs configure \
		; runhaskell Setup.hs build \
		; sudo runhaskell Setup.hs install \
		)

janus-srv	:
		( cd $(SRVDIR) \
		; runhaskell Setup.hs configure \
		; runhaskell Setup.hs build \
		; sudo runhaskell Setup.hs install \
		)

janus-plug	:
		make -C $(PLUGINDIR) all

janus-install	:
		$(MAKE) janus-lib
		$(MAKE) janus-srv
		$(MAKE) janus-plug

test-install	:
		[ -d "$(TESTDIR)" ] || mkdir   $(TESTDIR)
		cp -vr $(PLUGINDIR)/build/*    $(TESTDIR)
		cp -vr wwwpages                $(TESTDIR)
		cp -vr errorpages              $(TESTDIR)
		cp -vr $(SRVDIR)/examples/conf $(TESTDIR)

test-run	:
		( cd $(TESTDIR) ; janus ; )

# ------------------------------------------------------------


# DO NOT DELETE: Beginning of Haskell dependencies
build/Network/Server/HWS/Util.o : src/Network/Server/HWS/Util.hs
build/Network/Server/HWS/ParseError.o : src/Network/Server/HWS/ParseError.hs
build/Network/Server/HWS/Parser.o : src/Network/Server/HWS/Parser.hs
build/Network/Server/HWS/Parser.o : build/Network/Server/HWS/ParseError.hi
build/Network/Server/HWS/MimeTypes.o : src/Network/Server/HWS/MimeTypes.hs
build/Network/Server/HWS/Config.o : src/Network/Server/HWS/Config.hs
build/Network/Server/HWS/Config.o : build/Network/Server/HWS/MimeTypes.hi
build/Network/Server/HWS/Response.o : src/Network/Server/HWS/Response.hs
build/Network/Server/HWS/Response.o : build/Network/Server/HWS/MimeTypes.hi
build/Network/Server/HWS/Response.o : build/Network/Server/HWS/Util.hi
build/Network/Server/HWS/Response.o : build/Network/Server/HWS/Config.hi
build/Network/Server/HWS/Request.o : src/Network/Server/HWS/Request.hs
build/Network/Server/HWS/Request.o : build/Network/Server/HWS/Util.hi
build/Network/Server/HWS/Request.o : build/Network/Server/HWS/Response.hi
build/Network/Server/HWS/Request.o : build/Network/Server/HWS/Config.hi
build/Network/Server/HWS/Request.o : build/Network/Server/HWS/Parser.hi
build/Network/Server/Janus/JanusPaths.o : src/Network/Server/Janus/JanusPaths.hs
build/Network/Server/Janus/XmlHelper.o : src/Network/Server/Janus/XmlHelper.hs
build/Network/Server/Janus/XmlHelper.o : build/Network/Server/Janus/JanusPaths.hi
build/Network/Server/Janus/Messaging.o : src/Network/Server/Janus/Messaging.hs
build/Network/Server/Janus/Messaging.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Messaging.o : build/Network/Server/Janus/JanusPaths.hi
build/Network/Server/Janus/Transaction.o : src/Network/Server/Janus/Transaction.hs
build/Network/Server/Janus/Transaction.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Transaction.o : build/Network/Server/Janus/Messaging.hi
build/Network/Server/Janus/Transaction.o : build/Network/Server/Janus/JanusPaths.hi
build/Network/Server/Janus/HTMLBuilder.o : src/Network/Server/Janus/HTMLBuilder.hs
build/Network/Server/Janus/HTMLBuilder.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/DynamicLoader.o : src/Network/Server/Janus/DynamicLoader.hs
build/Network/Server/Janus/DynamicLoader.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Core.o : src/Network/Server/Janus/Core.hs
build/Network/Server/Janus/Core.o : build/Network/Server/Janus/JanusPaths.hi
build/Network/Server/Janus/Core.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Core.o : build/Network/Server/Janus/Transaction.hi
build/Network/Server/Janus/Core.o : build/Network/Server/Janus/Messaging.hi
build/Network/Server/Janus/Core.o : build/Network/Server/Janus/DynamicLoader.hi
build/Network/Server/Janus/ShopExample.o : src/Network/Server/Janus/ShopExample.hs
build/Network/Server/Janus/ShopExample.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/ShopExample.o : build/Network/Server/Janus/HTMLBuilder.hi
build/Network/Server/Janus/ShopExample.o : build/Network/Server/Janus/Core.hi
build/Network/Server/Janus/Handler/TCPHandler.o : src/Network/Server/Janus/Handler/TCPHandler.hs
build/Network/Server/Janus/Handler/TCPHandler.o : build/Network/Server/HWS/Util.hi
build/Network/Server/Janus/Handler/TCPHandler.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Handler/TCPHandler.o : build/Network/Server/Janus/Transaction.hi
build/Network/Server/Janus/Handler/TCPHandler.o : build/Network/Server/Janus/Core.hi
build/Network/Server/Janus/Handler/TCPHandler.o : build/Network/Server/Janus/Messaging.hi
build/Network/Server/Janus/Handler/ConsoleHandler.o : src/Network/Server/Janus/Handler/ConsoleHandler.hs
build/Network/Server/Janus/Handler/ConsoleHandler.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Handler/ConsoleHandler.o : build/Network/Server/Janus/Transaction.hi
build/Network/Server/Janus/Handler/ConsoleHandler.o : build/Network/Server/Janus/Messaging.hi
build/Network/Server/Janus/Handler/ConsoleHandler.o : build/Network/Server/Janus/Core.hi
build/Network/Server/Janus/Shader/ControlShader.o : src/Network/Server/Janus/Shader/ControlShader.hs
build/Network/Server/Janus/Shader/ControlShader.o : build/Network/Server/Janus/JanusPaths.hi
build/Network/Server/Janus/Shader/ControlShader.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Shader/ControlShader.o : build/Network/Server/Janus/Core.hi
build/Network/Server/Janus/Shader/DaemonShader.o : src/Network/Server/Janus/Shader/DaemonShader.hs
build/Network/Server/Janus/Shader/DaemonShader.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Shader/DaemonShader.o : build/Network/Server/Janus/Messaging.hi
build/Network/Server/Janus/Shader/DaemonShader.o : build/Network/Server/Janus/Core.hi
build/Network/Server/Janus/Shader/ExprShader.o : src/Network/Server/Janus/Shader/ExprShader.hs
build/Network/Server/Janus/Shader/ExprShader.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Shader/ExprShader.o : build/Network/Server/Janus/Core.hi
build/Network/Server/Janus/Shader/HTTPShader.o : src/Network/Server/Janus/Shader/HTTPShader.hs
build/Network/Server/Janus/Shader/HTTPShader.o : build/Network/Server/HWS/Util.hi
build/Network/Server/Janus/Shader/HTTPShader.o : build/Network/Server/HWS/Response.hi
build/Network/Server/Janus/Shader/HTTPShader.o : build/Network/Server/HWS/Request.hi
build/Network/Server/Janus/Shader/HTTPShader.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Shader/HTTPShader.o : build/Network/Server/Janus/Transaction.hi
build/Network/Server/Janus/Shader/HTTPShader.o : build/Network/Server/Janus/Messaging.hi
build/Network/Server/Janus/Shader/HTTPShader.o : build/Network/Server/Janus/Core.hi
build/Network/Server/Janus/Shader/PredicateShader.o : src/Network/Server/Janus/Shader/PredicateShader.hs
build/Network/Server/Janus/Shader/PredicateShader.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Shader/PredicateShader.o : build/Network/Server/Janus/Transaction.hi
build/Network/Server/Janus/Shader/PredicateShader.o : build/Network/Server/Janus/Messaging.hi
build/Network/Server/Janus/Shader/PredicateShader.o : build/Network/Server/Janus/Core.hi
build/Network/Server/Janus/Shader/ServletShader.o : src/Network/Server/Janus/Shader/ServletShader.hs
build/Network/Server/Janus/Shader/ServletShader.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Shader/ServletShader.o : build/Network/Server/Janus/HTMLBuilder.hi
build/Network/Server/Janus/Shader/ServletShader.o : build/Network/Server/Janus/Core.hi
build/Network/Server/Janus/Shader/ShaderLib.o : src/Network/Server/Janus/Shader/ShaderLib.hs
build/Network/Server/Janus/Shader/ShaderLib.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Shader/ShaderLib.o : build/Network/Server/Janus/Messaging.hi
build/Network/Server/Janus/Shader/ShaderLib.o : build/Network/Server/Janus/Core.hi
build/Network/Server/Janus/Shader/SOAPShader.o : src/Network/Server/Janus/Shader/SOAPShader.hs
build/Network/Server/Janus/Shader/SOAPShader.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Shader/SOAPShader.o : build/Network/Server/Janus/Core.hi
build/Network/Server/Janus/Shader/SystemShader.o : src/Network/Server/Janus/Shader/SystemShader.hs
build/Network/Server/Janus/Shader/SystemShader.o : build/Network/Server/Janus/JanusPaths.hi
build/Network/Server/Janus/Shader/SystemShader.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Shader/SystemShader.o : build/Network/Server/Janus/Messaging.hi
build/Network/Server/Janus/Shader/SystemShader.o : build/Network/Server/Janus/DynamicLoader.hi
build/Network/Server/Janus/Shader/SystemShader.o : build/Network/Server/Janus/Core.hi
build/Network/Server/Janus/Server.o : src/Network/Server/Janus/Server.hs
build/Network/Server/Janus/Server.o : build/Network/Server/Janus/JanusPaths.hi
build/Network/Server/Janus/Server.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Server.o : build/Network/Server/Janus/Transaction.hi
build/Network/Server/Janus/Server.o : build/Network/Server/Janus/Shader/ControlShader.hi
build/Network/Server/Janus/Server.o : build/Network/Server/Janus/Shader/SystemShader.hi
build/Network/Server/Janus/Server.o : build/Network/Server/Janus/Messaging.hi
build/Network/Server/Janus/Server.o : build/Network/Server/Janus/Core.hi
build/Network/Server/Janus/Shader/ConsoleShader.o : src/Network/Server/Janus/Shader/ConsoleShader.hs
build/Network/Server/Janus/Shader/ConsoleShader.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/Shader/ConsoleShader.o : build/Network/Server/Janus/Server.hi
build/Network/Server/Janus/Shader/ConsoleShader.o : build/Network/Server/Janus/Messaging.hi
build/Network/Server/Janus/Shader/ConsoleShader.o : build/Network/Server/Janus/DynamicLoader.hi
build/Network/Server/Janus/Shader/ConsoleShader.o : build/Network/Server/Janus/Core.hi
build/Network/Server/Janus/ContextBrowser.o : src/Network/Server/Janus/ContextBrowser.hs
build/Network/Server/Janus/ContextBrowser.o : build/Network/Server/Janus/XmlHelper.hi
build/Network/Server/Janus/ContextBrowser.o : build/Network/Server/Janus/HTMLBuilder.hi
build/Network/Server/Janus/ContextBrowser.o : build/Network/Server/Janus/Core.hi
# DO NOT DELETE: End of Haskell dependencies
