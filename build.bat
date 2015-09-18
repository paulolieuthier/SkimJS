
ghc -c PLC\fnv.c -o PLC\fnv.o
ghc Main.hs PLC\fnv.o -optP-include -optPPLC\cabal_macros.h -outputdir build