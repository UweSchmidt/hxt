# $Id: Makefile,v 1.1 2004/09/02 19:12:02 hxml Exp $
#
# Makefile to configure the http handlers to be used
#
# default is: http access may be done via a native http module
# or via an external program curl called via pipes
# the access can be controlled at runtime by option --withCurl
#
# variants:
#
# 1. ONLY_NATIVE_HTTP: does not need the code for calling external programs via pipes
# activate this variant by running "make only_native_http" in this dir before compilation
#
# 2. ONLY_CURL_HTTP: does not need the code for a socked connection and other stuff used by http modules
# activate this variant by running "make only_curl_http" in this dir before compilation
#
# 3. NO_HTTP: does not need any code for http, only local file access is supported
# activate this variant by running "make no_http" in this dir before compilation
#
# reset to default (both variants compiled in and selected by runtime options) by
# running "make default_http"

source	= ProtocolHandler.hs
tmp	= ProtocolHandler.tmp
target	= ../Parser/ProtocolHandler.hs

all		:
		@echo "make a target out of \"only_native_http only_curl_http no_http default_http\""

only_native_http	:
		ghc -E -cpp -DONLY_NATIVE_HTTP=1 -o $(tmp) $(source) ; grep -v '^#' $(tmp) > $(target)

only_curl_http	:
		ghc -E -cpp -DONLY_CURL_HTTP=1   -o $(tmp) $(source) ; grep -v '^#' $(tmp) > $(target)

no_http	:
		ghc -E -cpp -DNO_HTTP=1          -o $(tmp) $(source) ; grep -v '^#' $(tmp) > $(target)

default_http	:
		ghc -E -cpp                      -o $(tmp) $(source) ; grep -v '^#' $(tmp) > $(target)

clean		:
		rm -f $(tmp)

.PHONY		: all only_native_http only_curl_http no_http default_http clean
