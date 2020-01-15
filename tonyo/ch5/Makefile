exe=yajp7

all:
	ghc -O2 -o $(exe) json-main.hs src/*.hs
	rm *.hi *.o src/*.hi src/*.o 2>/dev/null
	prove -v test-suite.bash

allnoio:
	ghc -O2 -o "$(exe)-noio" json-main-noio.hs src/*.hs
	rm *.hi *.o src/*.hi src/*.o 2>/dev/null
	prove -v test-suite-noio.bash


test:
	prove -v test-suite.bash

clean:
	rm *.hi *.o src/*.hi src/*.o 2>/dev/null
