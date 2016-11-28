(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

type action = LLVM_IR | Compile (* | AST *)

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [
			      ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
			      ("-c", Compile) ] (* Generate, check LLVM IR *)
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  let cast = Organizer.convert ast in
  Semant.check cast;
  match action with
  | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate cast))
  | Compile -> let m = Codegen.translate cast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)
