# circling: test Makefile
#  - builds all files needed for testing, then runs tests

default: test

all:
	cd ..; make all

test: build
	bash ./test_scanner.sh

build:
	cd scanner; make
	cd parser; make

.PHONY: clean
clean:
	cd scanner; make clean
	cd parser; make clean
