boot:: 
	ghc --make -O2 Main.hs  -XMultiParamTypeClasses  -XFunctionalDependencies -XFlexibleInstances  -XFlexibleContexts 

#cbits.o -ffi -framework ApplicationServices
xxx::
	rm -f cbits.o
	gcc cbits.c -F/System/Library/Frameworks -c

