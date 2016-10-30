OBJS = parser.cmo scanner.cmo tokenize.cmo

testScanner : $(OBJS)
	ocamlc -o testScanner $(OBJS)

scanner.ml : ../../complier/scanner.mll
	ocamllex -o scanner.ml ../../complier/scanner.mll

tokenize.cmo : ../tokenize.ml
	ocamlc -o tokenize.cmo -c ../tokenize.ml

%.cmo : %.ml
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f testScanner scanner.ml *.cmo *.cmi
