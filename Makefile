all: main Library.o

main: main.hs Lexer.hs Parser.hs Semantics.hs Translator.hs Output.hs Types.hs 
	ghc --make -W main.hs

Lexer.hs: Lexer.x
	alex -g Lexer.x

Parser.hs: Parser.y
	happy -g Parser.y

Library.o: Library.asm
	nasm -f elf -o Library.o Library.asm

clean:
	rm -f *.o *.hi main

clean-all: clean
	rm -f Lexer.hs Parser.hs
