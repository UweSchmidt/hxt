all	: colorize

install	: all
	sudo cp colorize /usr/local/bin

clean	:
	rm -f *.o *.hi

distclean	: clean
		rm -f colorize

colorize	: ColorizeSourceCode.hs
		ghc -o $@ -O2 --make $<


