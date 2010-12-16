all: clean
	../test .

clean:
	rm -f *.asm *.o
	find . \! \( -name '*.*' -or -name 'Makefile' \) -exec rm {} \;
