all: lexer

lexer: lexer.hs
	ghc -o lexer lexer.hs

lexer.hs: lexer.x
	alex lexer.x

clean:
	rm *.o *.hi lexer.hs
