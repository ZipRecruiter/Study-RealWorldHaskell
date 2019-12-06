
# BASETYPES= Integral Floating Num Bool
BASETYPES= Monad

all: instances.svg instances.png

instances.png: instances.dot
	dot -Tpng instances.dot  > instances.png

instances.svg: instances.dot
	dot -Tsvg instances.dot  > instances.svg



instances.dot: instances.map
	cp instances.map instances.dot

instances.map: instances.py Makefile
	./instances.py $(BASETYPES) >  instances.map
