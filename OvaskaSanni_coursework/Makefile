all:
	happy -gca ParGram.y
	alex -g LexGram.x
	ghc --make TestGram.hs -o TestGram

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocGram.* LexGram.* ParGram.* LayoutGram.* SkelGram.* PrintGram.* TestGram.* AbsGram.* TestGram ErrM.* SharedString.* ComposOp.* Gram.dtd XMLGram.* Makefile*
	

