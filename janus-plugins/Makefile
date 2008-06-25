HC      	= ghc
HC_OPTS 	= -Wall -fglasgow-exts \
			-fno-warn-duplicate-exports \
			-fno-warn-deprecations \
			-fno-warn-overlapping-patterns \
			-farrows \
			-ibuild:source -odir build -hidir build

SRCBASE		= source/Network/Server/Janus
BUILDBASE	= build/Network/Server/Janus
DOCBASE		= temp/Network/Server/Janus

SRCS 		= $(SRCS_SHADER) $(SRCS_HANDLER)
SRCS_SHADER 	= $(SRCBASE)/Shader/ExprShader.hs $(SRCBASE)/Shader/HTTPShader.hs \
		  $(SRCBASE)/Shader/ConsoleShader.hs $(SRCBASE)/Shader/DaemonShader.hs \
		  $(SRCBASE)/Shader/PredicateShader.hs $(SRCBASE)/Shader/ServletShader.hs \
	          $(SRCBASE)/Shader/ShaderLib.hs $(SRCBASE)/Shader/SOAPShader.hs \
		  $(SRCBASE)/Shader/TestShader.hs
SRCS_HANDLER	= $(SRCBASE)/Handler/ConsoleHandler.hs $(SRCBASE)/Handler/SystemHandler.hs \
		  $(SRCBASE)/Handler/TCPHandler.hs

OBJS 		= $(OBJS_SHADER) $(OBJS_HANDLER)
OBJS_SHADER 	= $(BUILDBASE)/Shader/ExprShader.o $(BUILDBASE)/Shader/HTTPShader.o \
		  $(BUILDBASE)/Shader/ConsoleShader.o $(BUILDBASE)/Shader/DaemonShader.o \
		  $(BUILDBASE)/Shader/PredicateShader.o $(BUILDBASE)/Shader/ServletShader.o \
		  $(BUILDBASE)/Shader/ShaderLib.o $(SRCBASE)/Shader/SOAPShader.hs \
		  $(BUILDBASE)/Shader/TestShader.o
OBJS_HANDLER	= $(BUILDBASE)/Handler/ConsoleHandler.o $(BUILDBASE)/Handler/SystemHandler.o \
		  $(BUILDBASE)/Handler/TCPHandler.o

build/%.o: 	source/%.hs
		$(HC) -c $< $(HC_OPTS)

%.hi: 		%.o
		@:

all:		shader

shader:		./build $(OBJS)

build:
		mkdir build

temp/%.hs: 	src/%.hs
		mkdir -p $(@D)
		cat $< | sed -e 's/proc /\\/g' > $@

depend:
		ghc -M $(HC_OPTS) $(SRCS)

clean:
		rm -f build -r

build/Network/Server/Janus/Shader/ExprShader.o : source/Network/Server/Janus/Shader/ExprShader.hs
build/Network/Server/Janus/Shader/HTTPShader.o : source/Network/Server/Janus/Shader/HTTPShader.hs
build/Network/Server/Janus/Shader/PredicateShader.o : source/Network/Server/Janus/Shader/PredicateShader.hs
build/Network/Server/Janus/Shader/ServletShader.o : source/Network/Server/Janus/Shader/ServletShader.hs
build/Network/Server/Janus/Shader/ShaderLib.o : source/Network/Server/Janus/Shader/ShaderLib.hs
build/Network/Server/Janus/Shader/SOAPShader.o : source/Network/Server/Janus/Shader/SOAPShader.hs
build/Network/Server/Janus/Shader/TestShader.o : source/Network/Server/Janus/Shader/TestShader.hs

build/Network/Server/Janus/Handler/ConsoleHandler.o : source/Network/Server/Janus/Handler/ConsoleHandler.hs
build/Network/Server/Janus/Handler/SystemHandler.o : source/Network/Server/Janus/Handler/SystemHandler.hs
build/Network/Server/Janus/Handler/TCPHandler.o : source/Network/Server/Janus/Handler/TCPHandler.hs

