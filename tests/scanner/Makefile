# circline: test/scanner Makefile
OBJS = parser.cmo scanner.cmo tokenize.cmo

default : testScanner

all:
	cd ..; make all

testScanner : $(OBJS)
	ocamlc -o testScanner $(OBJS)

scanner.ml : ../../compiler/scanner.mll
	ocamllex -o scanner.ml ../../compiler/scanner.mll

tokenize.cmo : tokenize.ml
	ocamlc -o tokenize.cmo -c tokenize.ml

%.cmo : %.ml
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f testScanner scanner.ml *.cmo *.cmi
