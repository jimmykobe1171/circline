make clean
ocamlc -c ast.ml
ocamlc -c cast.ml
ocamlc -c organizer.ml
ocamlc -o pig ast.cmo cast.cmo organizer.cmo
./pig