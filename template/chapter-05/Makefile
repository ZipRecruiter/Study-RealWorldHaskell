# Example usage:
#   make
#   make run NAME=testmain
#   make build NAME=testmain
#   make clean

NAME ?= testmain

run: build
	./$(NAME)

build: $(NAME)

$(NAME): *.hs Makefile
	ghc $(NAME).hs

clean:
	rm -f *.hi *.o
