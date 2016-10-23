OBJS = parser.cmo scanner.cmo

testScanner : $(OBJS)
	ocamlc -o testScanner $(OBJS)

scanner.ml : ../../scanner.mll
	ocamllex -o scanner.ml ../../scanner.mll

%.cmo : %.ml
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f testScanner scanner.ml *.cmo *.cmi