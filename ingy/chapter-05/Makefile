# Example usage:
#   make
#   make test-lex
#   make test-parse
#   make run
#   make build
#   make clean

programs := \
    testmain \
    lex-json \
    parse-json \

run: testmain
	./$<

%: %.hs *.hs
	ghc $<

clean:
	rm -f *.hi *.o $(programs)

test-parse: parse-json
	printf '{' | ./$<

test-lex: lex-json
	printf '{true["foo\"" ,\n:0-1\t2.2\rfalse\n.3   -.4::5. -66.\n},' | ./$<
