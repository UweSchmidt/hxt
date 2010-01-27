# build a statically linked janus server
#
# all servlet sources must be located under source subdir
# and must be in the package Network.Server.Janus.Shader

SERVLETS	:= $(wildcard source/Network/Server/Janus/Shader/*.hs) \
		   $(wildcard source/Network/Server/Janus/Handler/*.hs)

CONF		= conf/server.xml

JANUSHOME	= ..

JANUSCONF	= $(JANUSHOME)/janus-library/dist/build/janus-conf/janus-conf 
# janus-conf is installed togeher with the janus-library

# JANUSCONF	= janus-conf

# generate the Janus main from server.xml

all		: janus

Janus.hs	: $(CONF)
		$(JANUSCONF) $(CONF)


# generate the executable janus from the main prog

janus		: Janus.hs $(SERVLETS)
		ghc -Wall -o $@ -XScopedTypeVariables -XArrows --make -i./source Janus.hs

clean		:
		rm -f Janus.hs Janus.o Janus.hi $(SERVLETS:.hs=.o) $(SERVLETS:.hs=.hi)
