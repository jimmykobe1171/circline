OBJS = parser.cmo scanner.cmo

default: all

all: $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f calc parser.ml parser.mli scanner.ml *.cmo *.cmi

parser.cmo: ast.cmi parser.cmi
parser.cmx: ast.cmi parser.cmi 
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
parser.cmi: ast.cmi
