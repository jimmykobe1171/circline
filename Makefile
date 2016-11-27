# circling: Makefile
#  - main entry point for building compiler and running tests

default: build

all: clean build

build:
	cd compiler; make
	cd tests; make build
	cd tests; sh test_scanner.sh
	cd tests; sh test_parser.sh
	cd tests; sh test_code_gen.sh

test: clean build
	cd tests; make

.PHONY: clean
clean:
	cd compiler; make clean
	cd tests; make clean
