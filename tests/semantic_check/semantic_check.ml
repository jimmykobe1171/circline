(* Program entry point *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  let cast = Organizer.convert ast in
  Semant.check cast;