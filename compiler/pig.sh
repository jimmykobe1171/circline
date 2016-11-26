make clean
ocamlc -c ast.ml
ocamlc -c cast.ml
ocamlyacc parser.mly
ocamllex scanner.mll
ocamlc -c parser.mli # compile parser types
ocamlc -c scanner.ml
ocamlc -c parser.ml
ocamlc -c organizer.ml
ocamlc -c parserize_cast.ml
ocamlc -o pig ast.cmo cast.cmo parser.cmo scanner.cmo organizer.cmo parserize_cast.cmo
#./pig