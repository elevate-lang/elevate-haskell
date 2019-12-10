all: clean
	ghc -dynamic *.hs && ./Main

clean: 
	rm -f *.hi *.o

i:
	ghci core.hs
