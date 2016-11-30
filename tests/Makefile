# circling: test Makefile
#  - builds all files needed for testing, then runs tests

default: test


test: clean build
	bash ./test_scanner.sh
	bash ./test_parser.sh
	bash ./test_semantic.sh
	bash ./test_code_gen.sh

build:
	cd scanner; make
	cd parser; make
	cd semantic_check; make

.PHONY: clean
clean:
	cd scanner; make clean
	cd parser; make clean
	cd semantic_check; make clean
