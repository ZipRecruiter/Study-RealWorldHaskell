# Example usage:
#   make
#   make test
#   make run NAME=testmain
#   make build NAME=testmain
#   make clean

NAME ?= lex-json

run: build
	./$(NAME)

build: $(NAME)

$(NAME): *.hs Makefile
	ghc $(NAME).hs

clean:
	rm -f *.hi *.o $(NAME)

test-lex: build
	printf '{true["foo" ,\n:0-1\t2.2\rfalse\n.3   -.4::5. -66.\n},' | \
	    ./lex-json
