# circling: Makefile
#  - main entry point for building compiler and running tests

default: build

all: clean build

build:
	cd compiler; make

test: build
	cd tests; make

.PHONY: clean
clean:
	cd compiler; make clean
	cd tests; make clean
