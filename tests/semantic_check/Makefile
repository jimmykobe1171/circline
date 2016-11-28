# test/semantic_check Makefile
#  - builds the semantic_check executable for semantic checking strings from stdin

OCAMLC = ocamlc
OBJS = ../../compiler/_build/parser.cmo ../../compiler/_build/scanner.cmo ../../compiler/_build/ast.cmo ../../compiler/_build/cast.cmo ../../compiler/_build/organizer.cmo ../../compiler/_build/semant.cmo semantic_check.cmo
INCLUDES = -I ../../compiler/_build

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
