# test/semantic_check Makefile
#  - builds the semantic_check executable for semantic checking strings from stdin

OCAMLC = ocamlc
OBJS = ../../compiler/parser.cmo ../../compiler/scanner.cmo ../../compiler/semant.cmo semantic_check.cmo
INCLUDES = -I ../../compiler

default: semantic_check

all:
	cd ..; make all

semantic_check: $(OBJS)
	$(OCAMLC) $(INCLUDES) -o semantic_check $(OBJS)

%.cmo: %.ml
	$(OCAMLC) $(INCLUDES) -c $<

%.cmi: %.mli
	$(OCAMLC) $(INCLUDES) -c $<

.PHONY: clean
clean:
	rm -f semantic_check *.cmo *.cmi
