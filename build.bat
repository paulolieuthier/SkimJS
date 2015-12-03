
ghc -c PLC\fnv.c -o PLC\fnv.o
ghc -c PLC\text_cbits.c -o PLC\text.o
ghc Main.hs PLC\fnv.o PLC\text.o -optP-include -optPPLC\definitions.h -outputdir build -XFlexibleInstances -XMultiParamTypeClasses -XFunctionalDependencies -XDeriveDataTypeable -XRankNTypes -XFlexibleContexts