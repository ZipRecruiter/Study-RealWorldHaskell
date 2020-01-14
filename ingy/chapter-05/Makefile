# Example usage:
#   make test
#   make test-lexer
#   make test-parser
#   make run
#   make clean

.DELETE_ON_ERROR:
programs := \
    testmain \
    json-lexer \
    json-parser \

run: testmain
	./$<

%: %.hs *.hs
	ghc $<

clean:
	rm -f *.hi *.o $(programs)

test: test-lexer test-parser

test-lexer: json-lexer
	@echo
	@echo json-lexer tests:
	@echo "* Malformed string of valid JSON tokens:"
	printf '{true["foo\"" ,\n:0-1\t2.2\rfalse\n.3   -.4::5. -66.\n},' | ./$<
	@echo "* Well formed JSON:"
	printf '[true,false,null,42,-3.14,"foo",{"a": [], "b": {}}]' | ./$<

test-parser: json-parser
	@echo
	@echo json-parser tests:
	@echo "* Valid JSON showing all forms:"
	printf '[true,false,null,42,-3.14,"foo",{"a": [], "b": {}}]' | ./$<
