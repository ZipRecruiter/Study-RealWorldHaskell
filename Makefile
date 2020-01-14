update: JSONTestSuite
	rm -f *.json
	mv $</test_parsing/*.json .
	rm -fr $<

JSONTestSuite:
	git clone --depth=1 https://github.com/nst/JSONTestSuite

clean:
	rm -fr JSONTestSuite
