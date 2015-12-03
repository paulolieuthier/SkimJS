EXTENSIONS = -XFlexibleInstances -XMultiParamTypeClasses \
	-XFunctionalDependencies -XDeriveDataTypeable \
	-XRankNTypes -XFlexibleContexts
OBJECTS = PLC/fnv.o PLC/text.o
INCLUDES = -optP-include -optPPLC/definitions.h

all:
	sh -c "if [ ! -d build ]; then mkdir build; fi"
	ghc -c PLC/fnv.c -o PLC/fnv.o
	ghc -c PLC/text_cbits.c -o PLC/text.o
	ghc Main.hs -outputdir build $(OBJECTS) $(EXTENSIONS) $(INCLUDES)

run:
	./Main

clean:
	rm -r build Main PLC/fnv.o
