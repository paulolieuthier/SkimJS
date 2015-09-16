
CFLAGS = -optP-include -optPPLC/cabal_macros.h

all: Main.hs
	sh -c "if [ ! -d build ]; then mkdir build; fi"
	ghc Main.hs PLC/fnv-unix.o $(CFLAGS) -outputdir build

run:
	./Main

clean:
	rm -r build Main
