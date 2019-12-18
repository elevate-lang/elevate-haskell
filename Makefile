all: clean
	ghc -dynamic -XGADTs *.hs && ./Main

clean: 
	rm -f *.hi *.o

i:
	ghci core.hs
