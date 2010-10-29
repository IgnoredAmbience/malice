all: main

main: main.hs Lexer.hs Parser.hs
	ghc --make main.hs

Lexer.hs: Lexer.x
	alex -g Lexer.x

Parser.hs: Parser.y
	happy -g Parser.y

clean:
	rm *.o *.hi Lexer.hs Parser.hs
