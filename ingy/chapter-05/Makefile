# Example usage:
#   make test
#   make test-lexer
#   make test-parser
#   make test-suite
#   make run
#   make clean

.DELETE_ON_ERROR:
programs := \
    testmain \
    json-lexer \
    json-parser

run: testmain
	./$<

%: %.hs *.hs
	ghc $<

clean:
	rm -f *.hi *.o $(programs)
	rm -fr json-test-suite

test-suite: json-parser json-test-suite
	prove -v test-suite.bash

json-test-suite:
	git worktree add -f $@ $@

test: test-lexer test-parser
	@echo

test-unit: json-parser
	printf '["x",]' | ./$< || true

test-lexer: json-lexer
	@echo
	@echo "===== json-lexer tests ========================================"
	@echo
	@echo "* Malformed string of valid JSON tokens:"
	printf '{true["foo\"" ,\n:0-1\t2.2\rfalse\n.3   -.4::5. -66.\n},' | ./$<
	@echo
	@echo "* Well formed JSON:"
	printf '[true,false,null,42,-3.14,"foo",{"a": [], "b": {}}]' | ./$<
	@echo
	@echo "* Error: unending string"
	printf '"x":"y' | ./$< || true
	@echo
	@echo "* Error: unknown character"
	printf ', +42' | ./$< || true
	@echo
	@echo "* Error: bad number"
	printf '.1, -.1, -.' | ./$< || true

test-parser: json-parser
	@echo
	@echo "===== json-parser tests ======================================="
	@echo
	@echo "* Array of possible value types:"
	printf '[{},[],"",-0,1.2,true,false,null]' | ./$<
	@echo
	@echo "* Valid JSON showing all forms:"
	printf '[true,false,null,42,-3.14,"foo",{"a": [], "b": {}}]' | ./$<
	@echo
	@echo "* Error: trailing comma"
	printf '{"foo":42,}' | ./$< || true
	@echo
	@echo "* Error: extra tokens"
	printf 'true,false' | ./$< || true
	@echo
	@echo "* Error: unended object"
	printf '[123,{"foo":42]' | ./$< || true
	@echo
	@echo "* Error: unended array"
	printf '{"foo":[123,}' | ./$< || true
