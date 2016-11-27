(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Cast

module StringMap = Map.Make(String)

let context = L.global_context ()
let the_module = L.create_module context "Circline"
and i32_t  = L.i32_type  context
and f_t  = L.double_type context
and i8_t   = L.i8_type   context
and i1_t   = L.i1_type   context
and str_t  = L.pointer_type (L.i8_type context)
and void_t = L.void_type context

let ltype_of_typ = function
    A.Int_t -> i32_t
  | A.Float_t -> f_t
  | A.Bool_t -> i1_t
  | A.String_t -> str_t
  | A.Void_t -> void_t
  | _ -> raise (Failure ("Type Not Found!"))

let get_default_value_of_type = function
  | A.Int_t as t -> L.const_int (ltype_of_typ t) 0
  | A.Bool_t as t -> L.const_int (ltype_of_typ t) 0
  | A.Float_t as t-> L.const_float (ltype_of_typ t) 0.
  | t-> L.const_null (ltype_of_typ t)

let codegen_string_lit s llbuilder =
  L.build_global_stringptr s "tmp" llbuilder

(*
================================================================
  Declare printf(), which the print built-in function will call
================================================================
*)
let printf_t = L.var_arg_function_type i32_t [| str_t |]
let printf_func = L.declare_function "printf" printf_t the_module
let codegen_print llbuilder el =
  L.build_call printf_func (Array.of_list el) "printf" llbuilder

(*
================================================================
  Casting
================================================================
*)
let int_to_float llbuilder v = L.build_sitofp v f_t "tmp" llbuilder

(*
================================================================
        context_funcs_vars
================================================================
*)
let context_funcs_vars = Hashtbl.create 50
let print_hashtbl tb =
  print_endline (Hashtbl.fold (fun k _ m -> (k^", "^m)) tb "")

(*
================================================================
        Main Codegen Function
================================================================
*)
let translate program =

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.name
      and formal_types =
	       Array.of_list (List.map (fun (A.Formal(t, _)) -> ltype_of_typ t) fdecl.A.args)
      in
      let ftype = L.var_arg_function_type (ltype_of_typ fdecl.A.returnType) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty program in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let get_var_name fname n = (fname ^ "." ^ n) in
    let (the_function, _) = StringMap.find fdecl.A.name function_decls in
    (* let bb = L.append_block context "entry" the_function in *)
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_to_context locals =
        ignore(Hashtbl.add context_funcs_vars fdecl.A.name locals);
        (* ignore(print_hashtbl context_funcs_vars); *)
        locals
      in
      let add_formal m (A.Formal(t, n)) p =
        let n' = get_var_name fdecl.A.name n in
        let local = L.define_global n' (get_default_value_of_type t) the_module in
        ignore (L.build_store p local builder);
        (* L.set_value_name n p;
    	  let local = L.build_alloca (ltype_of_typ t) n builder in
    	    ignore (L.build_store p local builder); *)
    	  StringMap.add n' (local, t) m
      in

      let add_local m (A.Formal(t, n)) =
        let n' = get_var_name fdecl.A.name n in
      	let local_var = L.define_global n' (get_default_value_of_type t) the_module in
      	(* let local_var = L.build_alloca (ltype_of_typ t) n builder in *)
        StringMap.add n' (local_var, t) m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.args
          (Array.to_list (L.params the_function)) in
      add_to_context (List.fold_left add_local formals fdecl.A.locals)
    in

    (* Return the value for a variable or formal argument *)
    (* let lookup n = StringMap.find n local_vars
    in *)
    let lookup n =
      let get_parent_func_name fname =
        let (_, fdecl) = StringMap.find fname function_decls in
        fdecl.A.pname
      in
      let rec aux n fname = (
        try StringMap.find (get_var_name fname n) (Hashtbl.find context_funcs_vars fname)
        with Not_found -> (
          if fname = "main" then
            (raise (Failure("Local Variable not found...")))
          else
            (aux n (get_parent_func_name fname))
        )
      ) in
      aux n fdecl.A.name
    in

    (* Return the type of expr *)
    let rec get_type = function
        A.Num_Lit(A.Num_Int _) -> A.Int_t
      | A.Num_Lit(A.Num_Float _) -> A.Float_t
      | A.String_Lit _ -> A.String_t
      | A.Bool_lit _ -> A.Bool_t
      | A.Id s -> let (_, typ) = lookup s in typ
      | A.Binop(e1, op, e2) -> let t1 = get_type e1 and t2 = get_type e2 in
          (
            match op with
            (* +,-,*,/ *)
              A.Add | A.Sub | A.Mult | A.Div when t1 = A.Int_t && t2 = A.Int_t -> A.Int_t
            | A.Add | A.Sub | A.Mult | A.Div when t1 = A.Float_t && t2 = A.Float_t -> A.Float_t
            | A.Add | A.Sub | A.Mult | A.Div when t1 = A.Int_t && t2 = A.Float_t -> A.Float_t
            | A.Add | A.Sub | A.Mult | A.Div when t1 = A.Float_t && t2 = A.Int_t -> A.Float_t
            (* =, != *)
            | A.Equal | A.Neq when t1 = t2 -> A.Bool_t
            (* <, <=, >, >= *)
            | A.Less | A.Leq | A.Greater | A.Geq when (t1 = A.Int_t || t1 = A.Float_t) && (t2 = A.Int_t || t2 = A.Float_t) -> A.Bool_t
            (* and, or *)
            | A.And | A.Or when t1 = A.Bool_t && t2 = A.Bool_t -> A.Bool_t
            (* mode *)
            | A.Mod when t1 = A.Int_t && t2 = A.Int_t -> A.Int_t
            | _ -> raise (Failure("Unrecognized Binop Operation..."))
          )
      | A.Unop(op, e) ->
          let t = get_type e in (
            match op with
              A.Neg when t = A.Int_t -> A.Int_t
            | A.Neg when t = A.Float_t -> A.Float_t
            | A.Not when t = A.Bool_t -> A.Bool_t
            | _ -> raise (Failure("Unrecognized Unop Operation..."))
          )
      | A.Assign(var, _) -> let (_, typ) = lookup var in typ
      | A.Call (f, _) ->
         let (_, fdecl) = StringMap.find f function_decls in
         fdecl.A.returnType
      | _ -> A.Void_t
    in
    (* Construct code for an expression; return its value *)
    let handle_binop e1 op e2 dtype llbuilder =
      (* Generate llvalues from e1 and e2 *)

      let float_ops op e1 e2 =
        match op with
          A.Add     -> L.build_fadd e1 e2 "flt_addtmp" llbuilder
        | A.Sub     -> L.build_fsub e1 e2 "flt_subtmp" llbuilder
        | A.Mult    -> L.build_fmul e1 e2 "flt_multmp" llbuilder
        | A.Div     -> L.build_fdiv e1 e2 "flt_divtmp" llbuilder
        | A.Mod     -> L.build_frem e1 e2 "flt_sremtmp" llbuilder
        | A.Equal   -> L.build_fcmp L.Fcmp.Oeq e1 e2 "flt_eqtmp" llbuilder
        | A.Neq     -> L.build_fcmp L.Fcmp.One e1 e2 "flt_neqtmp" llbuilder
        | A.Less    -> L.build_fcmp L.Fcmp.Ult e1 e2 "flt_lesstmp" llbuilder
        | A.Leq     -> L.build_fcmp L.Fcmp.Ole e1 e2 "flt_leqtmp" llbuilder
        | A.Greater -> L.build_fcmp L.Fcmp.Ogt e1 e2 "flt_sgttmp" llbuilder
        | A.Geq     -> L.build_fcmp L.Fcmp.Oge e1 e2 "flt_sgetmp" llbuilder
        | _ -> raise (Failure("Unrecognized float binop opreation!"))
      in

      (* chars are considered ints, so they will use int_ops as well*)
      let int_ops op e1 e2 =
        match op with
          A.Add     -> L.build_add e1 e2 "addtmp" llbuilder
        | A.Sub     -> L.build_sub e1 e2 "subtmp" llbuilder
        | A.Mult    -> L.build_mul e1 e2 "multmp" llbuilder
        | A.Div     -> L.build_sdiv e1 e2 "divtmp" llbuilder
        | A.Mod     -> L.build_srem e1 e2 "sremtmp" llbuilder
        | A.Equal   -> L.build_icmp L.Icmp.Eq e1 e2 "eqtmp" llbuilder
        | A.Neq     -> L.build_icmp L.Icmp.Ne e1 e2 "neqtmp" llbuilder
        | A.Less    -> L.build_icmp L.Icmp.Slt e1 e2 "lesstmp" llbuilder
        | A.Leq     -> L.build_icmp L.Icmp.Sle e1 e2 "leqtmp" llbuilder
        | A.Greater -> L.build_icmp L.Icmp.Sgt e1 e2 "sgttmp" llbuilder
        | A.Geq     -> L.build_icmp L.Icmp.Sge e1 e2 "sgetmp" llbuilder
        | A.And     -> L.build_and e1 e2 "andtmp" llbuilder
        | A.Or      -> L.build_or  e1 e2 "ortmp" llbuilder
        | _ -> raise (Failure("Unrecognized int binop opreation!"))
      in
      let type_handler d = match d with
        | A.Float_t -> float_ops op e1 e2
        | A.Bool_t
        | A.Int_t -> int_ops op e1 e2
        | _ -> raise (Failure("Unrecognized binop data type!"))
      in type_handler dtype
    in
    let rec expr builder = function
	      A.Num_Lit(A.Num_Int i) -> L.const_int i32_t i
      | A.Num_Lit(A.Num_Float f) -> L.const_float f_t f
      | A.Bool_lit b -> L.const_int i1_t (if b then 1 else 0)
      | A.String_Lit s -> codegen_string_lit s builder
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s ->
          let (var, _) = lookup s in
          L.build_load var s builder
      | A.Binop (e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        let t1 = get_type e1
        and t2 = get_type e2 in
        (* Handle Automatic Binop Type Converstion *)
        (match (t1, t2) with
          | ( t1, t2) when t1 = t2 -> handle_binop e1' op e2' t1 builder
          | ( A.Int_t, A.Float_t) ->
              handle_binop (int_to_float builder e1') op e2' A.Float_t builder
          | ( A.Float_t, A.Int_t ) ->
              handle_binop e1' op (int_to_float builder e2') A.Float_t builder
          | _ -> raise (Failure ("Unsuported Binop Type!"))
        )
      | A.Unop(op, e) ->
      	  let e' = expr builder e in
      	  (match op with
      	    A.Neg     -> if (get_type e) = A.Int_t then L.build_neg else L.build_fneg
          | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) ->
          let e' = expr builder e in
          let (var, typ) = lookup s in
          ( match (get_type e, typ) with
            | (t1, t2) when t1 = t2 -> ignore (L.build_store e' var builder); e'
            | (A.Int_t, A.Float_t) -> let e' = (int_to_float builder e') in ignore (L.build_store e' var builder); e'
            | _ -> raise (Failure("Assign Type inconsist"))
          )
      | A.Call ("print", el) ->
          let print_expr e = (
              match (get_type e) with
              | A.Int_t -> ignore(codegen_print builder [(codegen_string_lit "%d\n" builder); expr builder e])
              | A.Bool_t -> ignore(codegen_print builder [(codegen_string_lit "%d\n" builder); expr builder e])
                    (* (match (L.string_of_llvalue (expr builder e)) with
                    | "i1 true" -> ignore(codegen_print builder (List.map (fun s -> codegen_string_lit s builder) ["%s\n"; "true"]))
                    | _ -> ignore(codegen_print builder (List.map (fun s -> codegen_string_lit s builder) ["%s\n"; "false"]))) *)
              | A.Float_t -> ignore(codegen_print builder [(codegen_string_lit "%f\n" builder); expr builder e])
              (* | A.Bool_t -> ignore(codegen_print builder (List.map (fun s -> codegen_string_lit s builder) ["%d\n"; expr builder e]) *)
              | A.String_t -> ignore(codegen_print builder [(codegen_string_lit "%s\n" builder); expr builder e])
              | _ -> ignore(codegen_print builder (List.map (fun s -> codegen_string_lit s builder) ["%s\n"; "Unsuported Type for print..."]))
          ) in List.iter print_expr el; L.const_int i32_t 0
      | A.Call ("printf", el) ->
          codegen_print builder (List.map (expr builder) el)
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
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e ->
          ignore (
            let et = get_type e in
            match (fdecl.A.returnType, et) with
  	          (A.Void_t, _) -> L.build_ret_void builder
  	        | (t1, t2) when t1 = t2 -> L.build_ret (expr builder e) builder
            | (A.Float_t, A.Int_t) -> L.build_ret (int_to_float builder (expr builder e)) builder
            | _ -> raise (Failure("Return Type Doesn't match..."))
          ); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
      	 let merge_bb = L.append_block context "merge" the_function in

      	 let then_bb = L.append_block context "then" the_function in
      	 add_terminal (
             List.fold_left stmt (L.builder_at_end context then_bb) then_stmt
           ) (L.build_br merge_bb);

      	 let else_bb = L.append_block context "else" the_function in
         add_terminal (
             List.fold_left stmt (L.builder_at_end context else_bb) else_stmt
           ) (L.build_br merge_bb);

      	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
      	 L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
      	  let pred_bb = L.append_block context "while" the_function in
      	  ignore (L.build_br pred_bb builder);

      	  let body_bb = L.append_block context "while_body" the_function in
      	  add_terminal (
              List.fold_left stmt (L.builder_at_end context body_bb) body
            ) (L.build_br pred_bb);

      	  let pred_builder = L.builder_at_end context pred_bb in
      	  let bool_val = expr pred_builder predicate in

      	  let merge_bb = L.append_block context "merge" the_function in
      	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
      	  L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) -> List.fold_left stmt builder
	       ( [A.Expr e1 ; A.While (e2, body @ [A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = List.fold_left stmt builder fdecl.A.body in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.returnType with
        A.Void_t -> L.build_ret_void
      | A.Int_t as t -> L.build_ret (L.const_int (ltype_of_typ t) 0)
      | A.Bool_t as t -> L.build_ret (L.const_int (ltype_of_typ t) 0)
      | A.Float_t as t-> L.build_ret (L.const_float (ltype_of_typ t) 0.)
      | t-> L.build_ret (L.const_null (ltype_of_typ t))
    )
  in

  List.iter build_function_body program;

  the_module
