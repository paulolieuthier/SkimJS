EXTENSIONS = -XFlexibleInstances -XMultiParamTypeClasses \
	-XFunctionalDependencies -XDeriveDataTypeable \
	-XRankNTypes -XFlexibleContexts
OBJECTS = PLC/fnv.o PLC/text.o
INCLUDES = -optP-include -optPPLC/definitions.h

all:
	mkdir -p build
	ghc -fPIC -c PLC/fnv.c -o PLC/fnv.o
	ghc -fPIC -c PLC/text_cbits.c -o PLC/text.o
	ghc Main.hs -outputdir build $(OBJECTS) $(EXTENSIONS) $(INCLUDES)

run:
	./Main

clean:
	rm -rf build Main PLC/fnv.o PLC/text.o
