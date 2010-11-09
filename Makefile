all: main

main: main.hs Lexer.hs Parser.hs Semantics.hs Translator.hs Output.hs Types.hs 
	ghc --make main.hs

Lexer.hs: Lexer.x
	alex -g Lexer.x

Parser.hs: Parser.y
	happy -g Parser.y

clean:
	rm -f *.o *.hi main

clean-all: clean
	rm -f Lexer.hs Parser.hs
