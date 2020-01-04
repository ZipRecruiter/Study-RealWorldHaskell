CWD := $(shell pwd)
ROOT := $(CWD)/..
export PATH := $(ROOT)/bin:$(PATH)

TESTS := a-1 a-2 a-3 a-4 b-1 b-2 b-3 b-4 b-5 b-6
TESTS := $(TESTS:%=test-%)

test: $(TESTS)

test-%:
	ghcr $(@:test-%=%).hs test
	@echo

test-a-3:
	ghcr -i a-3.hs firstWords < a-3.hs
	@echo

test-a-4:
	ghcr -i a-4.hs transposeText < file1.txt
	@echo
