OBJS = parser.cmo scanner.cmo

foo : $(OBJS)
	ocamlc -o foo $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

%.cmo : %.ml
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f foo scanner.ml *.cmo *.cmi