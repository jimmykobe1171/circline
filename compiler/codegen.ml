(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
module P = Parserize

module StringMap = Map.Make(String)

let getLocals stmts =
  let aux l = function
    | A.Var_dec(A.Local(typ, name, _)) -> (typ, name)::l
    | _ -> l
  in List.rev( List.fold_left aux [] stmts )

let convertAST stmts =
  let (functions, variables, scripts) =
    let aux (fs, vars, others)= function
      | A.Func(_) as f -> (f::fs, vars, others)
      | A.Var_dec(A.Local(typ, name, v)) -> (
          match v with
          | A.Noexpr -> (fs, A.Var_dec(A.Local(typ, name, A.Noexpr))::vars, others)
          | _ -> (fs, A.Var_dec(A.Local(typ, name, A.Noexpr))::vars, A.Expr(A.Assign(name, v))::others)
        )
      | _ as other -> (fs, vars, other::others)
    in List.fold_left aux ([], [], []) stmts
  in (List.rev functions), (List.rev variables), (List.rev scripts)

let createMain stmts =
  let funcs, vars, scripts = convertAST stmts in
  funcs @ [ A.Func({
    returnType = Int_t;
    name = "main";
    args = [];
    body = vars @ scripts;
  }) ]

let translate raw_stmts =
  let stmts = createMain raw_stmts in
  let context = L.global_context () in
  let the_module = L.create_module context "MicroC"
  and i32_t  = L.i32_type  context
  and f32_t  = L.float_type context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in

  let ltype_of_typ = function
      A.Int_t -> i32_t
    | A.Float_t -> f32_t
    | A.Bool_t -> i1_t
    | A.Void_t -> void_t in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m (A.Func(fdecl)) =
      let name = fdecl.A.name
      and formal_types =
	       Array.of_list (List.map (fun (A.Formal(t, _)) -> ltype_of_typ t) fdecl.A.args)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.A.returnType) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty stmts in

  (* Fill in the body of the given function *)
  let build_function_body (A.Func(fdecl)) =
    let (the_function, _) = StringMap.find fdecl.A.name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (A.Formal(t, n)) p = L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
	ignore (L.build_store p local builder);
	StringMap.add n local m in

      let add_local m (t,n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.args
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals (getLocals fdecl.A.body) in

    (* Return the value for a variable or formal argument *)
    let lookup n = StringMap.find n local_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
	      A.Num_Lit(A.Num_Int i) -> L.const_int i32_t i
      | A.Num_Lit(A.Num_Float f) -> L.const_float f32_t f
      | A.Bool_lit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
      	  let e1' = expr builder e1
      	  and e2' = expr builder e2 in
      	  (match op with
      	    A.Add     -> L.build_add
      	  | A.Sub     -> L.build_sub
      	  | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
      	  | A.And     -> L.build_and
      	  | A.Or      -> L.build_or
      	  | A.Equal   -> L.build_icmp L.Icmp.Eq
      	  | A.Neq     -> L.build_icmp L.Icmp.Ne
      	  | A.Less    -> L.build_icmp L.Icmp.Slt
      	  | A.Leq     -> L.build_icmp L.Icmp.Sle
      	  | A.Greater -> L.build_icmp L.Icmp.Sgt
      	  | A.Geq     -> L.build_icmp L.Icmp.Sge
      	  ) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
      	  let e' = expr builder e in
      	  (match op with
      	    A.Sub     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) -> let e' = expr builder e in
	                   ignore (L.build_store e' (lookup s) builder); e'
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
	         L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
      	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
      	 let result = (match fdecl.A.returnType with A.Void_t -> ""
                                                   | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
      | _ -> L.const_int i32_t 0
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	      Some _ -> ()
      | None -> ignore (f builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
      | A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.returnType with
	          A.Void_t -> L.build_ret_void builder
	        | _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
      	 let merge_bb = L.append_block context "merge" the_function in

      	 let then_bb = L.append_block context "then" the_function in
      	 add_terminal (stmt (L.builder_at_end context then_bb) (A.Block then_stmt))
      	   (L.build_br merge_bb);

      	 let else_bb = L.append_block context "else" the_function in
      	 add_terminal (stmt (L.builder_at_end context else_bb) (A.Block else_stmt))
      	   (L.build_br merge_bb);

      	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
      	 L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
      	  let pred_bb = L.append_block context "while" the_function in
      	  ignore (L.build_br pred_bb builder);

      	  let body_bb = L.append_block context "while_body" the_function in
      	  add_terminal (stmt (L.builder_at_end context body_bb) (A.Block body))
      	    (L.build_br pred_bb);

      	  let pred_builder = L.builder_at_end context pred_bb in
      	  let bool_val = expr pred_builder predicate in

      	  let merge_bb = L.append_block context "merge" the_function in
      	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
      	  L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) -> stmt builder
	       ( A.Block [A.Expr e1 ; A.While (e2, body @ [A.Expr e3]) ] )
      | A.Var_dec(_) -> builder
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.returnType with
        A.Void_t -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  (* print_endline (P.string_of_program (stmts)); *)
  List.iter build_function_body stmts;
  the_module
