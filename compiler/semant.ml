open Cast

module StringMap = Map.Make(String)

(* Pretty-printing functions *)
let string_of_typ = function
    Int_t -> "int"
  | Float_t -> "float"
  | String_t -> "string"
  | Bool_t -> "bool" 
  | Node_t -> "node" 
  | Graph_t -> "graph" 
  | List_Int_t -> "list<int>"
  | List_Float_t -> "list<float>"
  | List_String_t -> "list<string>"
  | List_Node_t -> "list<node>"
  | List_Graph_t -> "list<graph>"
  | Dict_Int_t -> "dic<int>" 
  | Dict_Float_t -> "dic<float>" 
  | Dict_String_t -> "dic<string>" 
  | Dict_Node_t -> "dic<node>" 
  | Dict_Graph_t -> "dic<graph>" 
  | Void_t -> "void"
  | Null_t -> "null"

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"
  | ListNodesAt -> "@"
  | ListEdgesAt -> "@@"
  | RootAs -> "~"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_graph_op = function
    Right_Link -> "->"
  | Left_Link -> "<-"
  | Double_Link -> "--"

let rec string_of_expr = function
    Num_Lit(Num_Int(l)) -> string_of_int l
  | Num_Lit(Num_Float(l)) -> string_of_float l
  | Null -> "null"
  | String_Lit(l) -> l
  | Bool_lit(true) -> "true"
  | Bool_lit(false) -> "false"
  | Node(e) -> "node(" ^ string_of_expr e ^ ")"
  | Graph_Link(e1, op, e2, e3) -> 
      "graph_link(" ^ string_of_expr e1 ^ " " ^ string_of_graph_op op ^ " " ^ string_of_expr e2 ^ " " ^ string_of_expr e3 ^ ")"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Id(s) -> s
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Noexpr -> ""
  (* TODO: maybe revise to a more meaningful name *)
  | ListP(es) -> "list" 
  | DictP(es) -> "dict"
  | Call(n, es) -> "function call " ^ n
  | CallDefault(e, n, es) -> "function call " ^ string_of_expr e ^ "." ^ n
  

let  match_list_type = function
  Int_t -> List_Int_t
| Float_t -> List_Float_t
| String_t -> List_String_t
| Node_t -> List_Node_t
| Graph_t -> List_Graph_t
| _ as t-> raise (Failure("invalid list type: " ^ string_of_typ t))

let  match_dict_type = function
  Int_t -> Dict_Int_t
| Float_t -> Dict_Float_t
| String_t -> Dict_String_t
| Node_t -> Dict_Node_t
| Graph_t -> Dict_Graph_t
| _ as t-> raise (Failure("invalid dict type: " ^ string_of_typ t))

let check_valid_list_type typ =
    if typ = List_Int_t || typ = List_Float_t || typ = List_String_t || typ = List_Node_t || typ = List_Graph_t then typ
    else raise (Failure("invalid list type: " ^ string_of_typ typ))

let check_valid_dict_type typ =
    if typ = Dict_Int_t || typ = Dict_Float_t || typ = Dict_String_t || typ = Dict_Node_t || typ = Dict_Graph_t then typ
    else raise (Failure("invalid dict type: " ^ string_of_typ typ))

(* get function obj from func_map, if not found, raise error *)
let get_func_obj name func_map = 
    try StringMap.find name func_map
    with Not_found -> raise (Failure ("undeclared function " ^ name))


(* Raise an exception if the given list has a duplicate *)
let report_duplicate exceptf list =
    let rec helper = function
        n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
        | _ :: t -> helper t
        | [] -> ()
    in helper (List.sort compare list)

(* check function *)
let check_function func_map func =
    (* check duplicate formals *)
    let args = List.map (fun (Formal(t, n)) -> n) func.args in
    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.name) args;

    (* check duplicate locals *)
    let locals = List.map (fun (Formal(t, n)) -> n) func.locals in
    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.name) locals;

    let symbols = List.fold_left (fun m (Formal(t, n)) -> StringMap.add n t m)
        StringMap.empty (func.args @ func.locals )
    in
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
    (* Raise an exception of the given rvalue type cannot be assigned to
    he given lvalue type *)
    let check_assign lvaluet rvaluet err =
        if lvaluet == rvaluet then lvaluet else raise err
    in
    (* Return the type of an expression or throw an exception *)
    let rec expr = function
          Num_Lit(Num_Int _) -> Int_t 
        | Num_Lit(Num_Float _) -> Float_t
        | Null -> Null_t
        | String_Lit _ -> String_t 
        | Bool_lit _ -> Bool_t
        (* TODO: check node and graph *)
        | Node(e) -> Node_t
        | Graph_Link(e1, op, e2, e3) -> Graph_t
        | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
            (match op with
            (* +,-,*,/ *)
            Add | Sub | Mult | Div when t1 = Int_t && t2 = Int_t -> Int_t
            |Add | Sub | Mult | Div when t1 = Float_t && t2 = Float_t -> Float_t
            |Add | Sub | Mult | Div when t1 = Int_t && t2 = Float_t -> Float_t
            |Add | Sub | Mult | Div when t1 = Float_t && t2 = Int_t -> Float_t
            (* =, != *)
            | Equal | Neq when t1 = t2 -> Bool_t
            (* <, <=, >, >= *)
            | Less | Leq | Greater | Geq when (t1 = Int_t || t1 = Float_t) && (t2 = Int_t || t2 = Float_t) -> Bool_t
            (* and, or *)
            | And | Or when t1 = Bool_t && t2 = Bool_t -> Bool_t
            (* mode *)
            | Mod when t1 = Int_t && t2 = Int_t -> Int_t
            | _ -> raise (Failure ("illegal binary operator " ^
                      string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                      string_of_typ t2 ^ " in " ^ string_of_expr e))
            )
        | Unop(op, e) as ex -> let t = expr e in
            (match op with
            Neg when t = Int_t -> Int_t
            |Neg when t = Float_t -> Float_t
            | Not when t = Bool_t -> Bool_t
            | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
                    string_of_typ t ^ " in " ^ string_of_expr ex)))
        | Id s -> type_of_identifier s
        (* check assignment operation, check cases of empty list and empty dict *)
        | Assign(var, ListP([])) -> let lt = type_of_identifier var in check_valid_list_type lt
        | Assign(var, DictP([])) -> let lt = type_of_identifier var in check_valid_dict_type lt
        | Assign(var, e) as ex -> let lt = type_of_identifier var and rt = expr e in
            check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
            " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex))
        | Noexpr -> Void_t
        (* check list element type, if empty, assign List_Int_t here, and check again when encounter
        assignment operation *)
        | ListP([]) -> List_Int_t
        | ListP(es) -> 
            let element_type =
              let determine_element_type ss = List.fold_left 
                (fun l e -> (match l with
                  [] -> [expr e]
                | [t] when t = (expr e) -> [t]
                | _ -> raise (Failure ("list can not contain objects of different types")))
                ) [] ss
              in
              List.hd (determine_element_type es)
            in
            match_list_type element_type
        (* check dictionary element type if empty, assign Dict_Int_t here, and check again when encounter
        assignment operation *)
        | DictP([]) -> Dict_Int_t
        | DictP(es) ->
            let element_type =
              let determine_element_type ss = List.fold_left 
                (fun l (n, e) -> (match l with
                  [] -> [expr e]
                | [t] when t = (expr e) -> [t]
                | _ -> raise (Failure ("dict can not contain objects of different types")))
                ) [] ss
              in
              List.hd (determine_element_type es)
            in
            match_dict_type element_type
        | Call(n, args) -> let func_obj = get_func_obj n func_map in
              (* check function call such as the args length, args type *)
              let check_funciton_call func args =
                  let check_args_length l_arg r_arg = if (List.length l_arg) = (List.length r_arg)
                      then () else raise (Failure("args length not match in function call: " ^ func.name))
                  in
                  check_args_length func.args args;
                  (* l_arg is a list of Formal(typ, name), r_arg is a list of expr *)
                  let check_args_type l_arg r_arg =
                      List.map2 
                          (fun (Formal(t, n)) r -> let r_typ = expr r in if t = r_typ then () else
                          raise (Failure("incompatible argument type " ^ string_of_typ t ^ " but " ^ string_of_typ r_typ ^ " is expected"))) 
                          l_arg r_arg
                  in
                  check_args_type func.args args
              in
              ignore(check_funciton_call func_obj args); func_obj.returnType
        (* |   CallDefault(e, n, es) -> *)
    in
    (* check statement *)
    let stmt = function
            Expr(e) -> ignore (expr e)
            | Return e -> ignore (expr e)
            (* | Var_dec(Local(typ, name, e)) when e <> Noexpr -> let lt = typ and rt = expr e in
                ignore(check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
                " = " ^ string_of_typ rt ^ " in " ^ string_of_typ lt ^ " " ^ name ^ " = " ^ string_of_expr e)))
            | Var_dec(Local(typ, name, e)) when e = Noexpr -> () *)
    in
    (* check statement list *)
    let rec stmt_list = function
            Return _ :: _ -> raise (Failure "nothing may follow a return")
            | s::ss -> stmt s ; stmt_list ss
            | [] -> ()

    in
    stmt_list func.body

(* program here is a list of functions *)
let check program =
    let rec collect_functions m = function
        [] -> m
        | f :: t -> ignore(StringMap.add f.name f m); collect_functions m t
        | _ :: t -> collect_functions m t
    in
    (* collect global variable declarations *)
    let func_map = collect_functions StringMap.empty program in
(*     (* loop through the stmt_list of program to collect global variable declarations *)
    let rec collect_globals list = function
        [] -> list
        | Var_dec(Local(typ, name, v)) :: t -> (name, (typ, v)) :: collect_globals list t
        | _ :: t -> collect_globals list t
    in
    (* collect global variable declarations *)
    let globals = collect_globals [] program in

    (* Type and value of global variable *)
    let globals_symbols = List.fold_left (fun m (name, (typ, v)) -> StringMap.add name (typ, v) m)
        StringMap.empty globals
    in
    let type_of_identifier s =
      try fst (StringMap.find s globals_symbols)
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
    
    (**** Checking duplicated global variables ****)
    report_duplicate (fun n -> "duplicate global " ^ n) (List.map fst globals); *)

    (**** Checking statements ****)
    (* stmt_list program; *)

    
    (**** Checking functions ****)
    let check_function_wrapper func m =
        func m
    in
    List.iter (check_function_wrapper check_function func_map) program
