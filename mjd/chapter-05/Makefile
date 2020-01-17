exe=json-main

all:
	ghc -O2 -o $(exe) json-main.hs
#	rm *.hi *.o src/*.hi src/*.o 2>/dev/null
#	ghc -O2 -o "$(exe)-noio" json-main-noio.hs src/*.hs
#	rm *.hi *.o src/*.hi src/*.o 2>/dev/null

test:
	prove -v test-suite-noio.bash

testio:
	prove -v test-suite.bash

clean:
	rm *.hi *.o src/*.hi src/*.o 2>/dev/null
