
all:
	sh -c "if [ ! -d build ]; then mkdir build; fi"
	ghc -c PLC/fnv.c -o PLC/fnv.o
	ghc Main.hs PLC/fnv.o -optP-include -optPPLC/cabal_macros.h -outputdir build

run:
	./Main

clean:
	rm -r build Main PLC/fnv.o
